# translateFeatureTable(): the expansion and collapse step compareSignatures()
# applies to a signature's feature table and difexp when map_orthologs = TRUE.
# Pure; no database.

tbl <- data.frame(
  feature_name = c("a", "b", "c", "d"), score = c(2, -3, 1, 5),
  group_label = c("Up", "Dn", "Up", "Up"), stringsAsFactors = FALSE
)
# "a" and "c" both land on X (c also on Z); "d" has no ortholog.
mapping <- data.frame(query = c("A", "b", "c", "c"), target = c("X", "Y", "X", "Z"), stringsAsFactors = FALSE)

test_that("translateFeatureTable expands one feature to all its orthologs", {
  out <- translateFeatureTable(tbl, mapping, "feature_name", "group_label", prefer_col = "score", prefer = "max_abs")
  expect_setequal(out$table$feature_name, c("X", "Y", "Z"))
  expect_equal(out$n_input, 4)
  expect_equal(out$n_unmapped, 1)
  expect_equal(out$n_output, 3)
})

test_that("translateFeatureTable collapses duplicates per group keeping the largest absolute score", {
  out <- translateFeatureTable(tbl, mapping, "feature_name", "group_label", prefer_col = "score", prefer = "max_abs")
  x <- out$table[out$table$feature_name == "X", ]
  expect_equal(nrow(x), 1)
  expect_equal(x$score, 2)   # a (2) beats c (1) inside group Up
  expect_equal(x$group_label, "Up")
})

test_that("translateFeatureTable keeps duplicates apart when they sit in different groups", {
  two_groups <- tbl
  two_groups$group_label[3] <- "Dn"     # c now in Dn, a stays in Up; both map to X
  out <- translateFeatureTable(two_groups, mapping, "feature_name", "group_label", prefer_col = "score", prefer = "max_abs")
  expect_equal(sum(out$table$feature_name == "X"), 2)
})

test_that("translateFeatureTable keeps the smallest p-value when asked", {
  dif <- data.frame(feature_name = c("a", "c"), p_value = c(0.5, 0.01), stringsAsFactors = FALSE)
  out <- translateFeatureTable(dif, mapping, "feature_name", NULL, prefer_col = "p_value", prefer = "min")
  expect_equal(out$table$p_value[out$table$feature_name == "X"], 0.01)
  expect_equal(out$n_output, 2)   # X and Z
})

test_that("translateFeatureTable matches queries case-insensitively and trimmed", {
  spaced <- tbl
  spaced$feature_name <- c(" a ", "B", "c", "d")
  out <- translateFeatureTable(spaced, mapping, "feature_name", "group_label", prefer_col = "score", prefer = "max_abs")
  expect_setequal(out$table$feature_name, c("X", "Y", "Z"))
})

test_that("translateFeatureTable without a prefer column keeps the first row", {
  out <- translateFeatureTable(tbl[, c("feature_name", "group_label")], mapping, "feature_name", "group_label", prefer_col = "score", prefer = "max_abs")
  expect_equal(sum(out$table$feature_name == "X"), 1)
  expect_equal(out$n_output, 3)
})

test_that("translateFeatureTable returns an empty table when nothing maps", {
  out <- translateFeatureTable(tbl, mapping[0, ], "feature_name", "group_label", prefer_col = "score", prefer = "max_abs")
  expect_equal(nrow(out$table), 0)
  expect_equal(colnames(out$table), colnames(tbl))
  expect_equal(out$n_unmapped, 4)
})
