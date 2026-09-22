# The WHERE clause lookup_table_sql() sends to MySQL. DBI::ANSI() is used so
# the SQL can be checked without a database: it escapes a single quote by
# doubling it, whereas an RMySQL connection escapes it with a backslash. Both
# are safe; these tests pin ANSI's output. Each filter is a pair -- the bare
# column first, so MySQL can use its index (trim(lower(col)) alone turned a
# 2 ms lookup into a 1.3 s full table scan), then the original
# trim(lower(col)) comparison, so exactly the rows the old query matched
# are returned.

where_clause <- function(...) SigRepo:::build_lookup_where_clause(DBI::ANSI(), ...)

test_that("a filter pairs the bare column with the original comparison", {
  expect_identical(
    where_clause("signature_id", list(signature_id = 275)),
    "(signature_id IN ('275') AND trim(lower(signature_id)) IN ('275'))"
  )
})

test_that("each filter starts with the bare column, which lets MySQL use the index", {
  clause <- where_clause(c("signature_id", "user_name"), list(signature_id = 1, user_name = "devadmin"), "AND")
  for(col in c("signature_id", "user_name")){
    bare <- regexpr(paste0("(", col, " IN ("), clause, fixed = TRUE)
    wrapped <- regexpr(paste0("trim(lower(", col, ")) IN ("), clause, fixed = TRUE)
    expect_true(bare > 0)
    expect_true(wrapped > 0)
    expect_true(bare < wrapped)
  }
})

test_that("values are trimmed and lower-cased as the original lookup did", {
  expect_identical(
    where_clause("organism", list(organism = "  Homo Sapiens ")),
    "(organism IN ('homo sapiens') AND trim(lower(organism)) IN ('homo sapiens'))"
  )
})

test_that("every value is quoted, including quotes and injection attempts", {
  clause <- where_clause("user_name", list(user_name = c("O'Brien", "x'); DROP TABLE users; --")))
  expect_identical(
    clause,
    "(user_name IN ('o''brien', 'x''); drop table users; --') AND trim(lower(user_name)) IN ('o''brien', 'x''); drop table users; --'))"
  )
})

test_that("a filter with no values matches nothing instead of producing invalid SQL", {
  expect_identical(where_clause("signature_id", list(signature_id = character())), "1 = 0")
})

test_that("several filters are joined with the given logical operators", {
  expect_identical(
    where_clause(
      c("signature_id", "user_name", "access_type"),
      list(signature_id = c(1, 2), user_name = "devadmin", access_type = "owner"),
      c("AND", "OR")
    ),
    paste(
      "(signature_id IN ('1', '2') AND trim(lower(signature_id)) IN ('1', '2'))",
      "AND (user_name IN ('devadmin') AND trim(lower(user_name)) IN ('devadmin'))",
      "OR (access_type IN ('owner') AND trim(lower(access_type)) IN ('owner'))"
    )
  )
})

# ---- partial matching (issues #230, #231) -----------------------------------

# Named columns narrow with LIKE instead of IN. That is only ever a superset:
# search_match_rows() decides the final rows in R, because "exact wins if one
# exists" cannot be expressed per value in a single WHERE clause. Strictly
# opt-in -- lookup_table_sql() is the same function that fetches 15,000
# signature_feature_set rows by id, where a leading wildcard would throw away
# the index this clause exists to use.

test_that("a partial column narrows with LIKE rather than IN", {
  expect_identical(
    where_clause("signature_name", list(signature_name = "M005"),
                 partial_match_columns = "signature_name"),
    "(trim(lower(signature_name)) LIKE '%m005%' ESCAPE '|')"
  )
})

test_that("several terms in a partial column are OR-ed", {
  expect_identical(
    where_clause("signature_name", list(signature_name = c("M005", "LLFS")),
                 partial_match_columns = "signature_name"),
    "(trim(lower(signature_name)) LIKE '%m005%' ESCAPE '|' OR trim(lower(signature_name)) LIKE '%llfs%' ESCAPE '|')"
  )
})

test_that("a column not named keeps the exact clause it had", {
  expect_identical(
    where_clause("signature_id", list(signature_id = 275),
                 partial_match_columns = "signature_name"),
    "(signature_id IN ('275') AND trim(lower(signature_id)) IN ('275'))"
  )
})

test_that("naming no columns leaves every clause exactly as before", {
  expect_identical(
    where_clause("organism", list(organism = "Mus musculus"), partial_match_columns = character()),
    where_clause("organism", list(organism = "Mus musculus"))
  )
})

test_that("a percent sign in a term searches for a percent sign", {
  # Otherwise a phenotype written "top 5% by score" would match everything.
  expect_identical(
    where_clause("phenotype", list(phenotype = "top 5% by score"),
                 partial_match_columns = "phenotype"),
    "(trim(lower(phenotype)) LIKE '%top 5|% by score%' ESCAPE '|')"
  )
})

test_that("an underscore in a term searches for an underscore", {
  # Signature names are full of them, and LIKE reads a bare _ as any character.
  expect_identical(
    where_clause("signature_name", list(signature_name = "LC_M005"),
                 partial_match_columns = "signature_name"),
    "(trim(lower(signature_name)) LIKE '%lc|_m005%' ESCAPE '|')"
  )
})

test_that("the escape character itself is escaped", {
  expect_identical(
    where_clause("keywords", list(keywords = "a|b"), partial_match_columns = "keywords"),
    "(trim(lower(keywords)) LIKE '%a||b%' ESCAPE '|')"
  )
})

test_that("a quote in a partial term is still quoted safely", {
  expect_identical(
    where_clause("signature_name", list(signature_name = "O'Brien"),
                 partial_match_columns = "signature_name"),
    "(trim(lower(signature_name)) LIKE '%o''brien%' ESCAPE '|')"
  )
})

test_that("partial and exact columns combine with the given operators", {
  expect_identical(
    where_clause(
      c("signature_name", "organism_id"),
      list(signature_name = "M005", organism_id = 1),
      "AND",
      partial_match_columns = "signature_name"
    ),
    paste(
      "(trim(lower(signature_name)) LIKE '%m005%' ESCAPE '|')",
      "AND (organism_id IN ('1') AND trim(lower(organism_id)) IN ('1'))"
    )
  )
})

test_that("a partial column with no values still matches nothing", {
  expect_identical(
    where_clause("signature_name", list(signature_name = character()),
                 partial_match_columns = "signature_name"),
    "1 = 0"
  )
})

test_that("thousands of values all reach both predicates in order", {
  ids <- as.character(seq_len(15000))
  values <- paste0("'", ids, "'", collapse = ", ")
  clause <- where_clause("feature_id", list(feature_id = ids))
  expect_identical(
    clause,
    paste0("(feature_id IN (", values, ") AND trim(lower(feature_id)) IN (", values, "))")
  )
})
