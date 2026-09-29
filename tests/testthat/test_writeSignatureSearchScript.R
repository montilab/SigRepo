# writeSignatureSearchScript() writes an R script and runs nothing, so these
# tests need neither a database nor signatureSearch. They read the query back
# out of the written script instead of trusting the function's return value.

lincs_sig_table <- function(n_up = 12, n_down = 12) {
  n <- n_up + n_down
  data.frame(
    probe_id = paste0("p", seq_len(n)),
    feature_name = sprintf("ENSG%011d", seq_len(n)),
    score = c(seq_len(n_up), -seq_len(n_down)),
    group_label = factor(c(rep("Old", n_up), rep("Young", n_down))),
    stringsAsFactors = FALSE
  )
}

make_lincs_sig <- function(signature = lincs_sig_table(), ...) {
  make_hyper_sig(name = "lincs_sig", signature = signature, ...)
}

local_script_path <- function(env = parent.frame()) {
  withr::local_tempfile(fileext = ".R", .local_envir = env)
}

# The values the script assigns at top level, without running the search.
script_values <- function(path) {
  wanted <- c("signature_name", "id_type", "up_ids", "down_ids", "n_genes", "output_file", "plot_file")
  env <- new.env()
  for (expr in parse(path)) {
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) && as.character(expr[[2]])[1] %in% wanted) {
      eval(expr, env)
    }
  }
  as.list(env)
}

test_that("the script holds each direction's features, strongest score first", {
  path <- local_script_path()
  SigRepo::writeSignatureSearchScript(omic_signature = make_lincs_sig(), file = path, verbose = FALSE)

  values <- script_values(path)
  expect_equal(values$signature_name, "lincs_sig")
  expect_equal(values$up_ids, sprintf("ENSG%011d", 12:1))
  expect_equal(values$down_ids, sprintf("ENSG%011d", 24:13))
})

test_that("the script runs the LINCS search and saves the result", {
  path <- local_script_path()
  SigRepo::writeSignatureSearchScript(omic_signature = make_lincs_sig(), file = path, verbose = FALSE)

  called <- all.names(parse(path))
  expect_true(all(c("qSig", "gess_lincs", "write.csv") %in% called))
})

test_that("the script plots the strongest hits to a PDF named after the signature", {
  path <- local_script_path()
  SigRepo::writeSignatureSearchScript(omic_signature = make_lincs_sig(), file = path, verbose = FALSE)

  expect_equal(script_values(path)$output_file, "lincs_sig_lincs_results.csv")
  expect_equal(script_values(path)$plot_file, "lincs_sig_lincs_top_hits.pdf")
  expect_true(all(c("gess_res_vis", "ggsave") %in% all.names(parse(path))))
})

# gess_lincs() annotates its hits with data sets it loads by name, which R
# only finds in an attached package: called as signatureSearch::gess_lincs()
# alone, the search runs and then fails with "object 'clue_moa_list' not found".
test_that("the script attaches signatureSearch", {
  path <- local_script_path()
  SigRepo::writeSignatureSearchScript(omic_signature = make_lincs_sig(), file = path, verbose = FALSE)

  expect_match(
    paste(deparse(parse(path)), collapse = "\n"),
    "library(signatureSearch)",
    fixed = TRUE
  )
})

test_that("the path is returned invisibly", {
  path <- local_script_path()
  out <- withVisible(SigRepo::writeSignatureSearchScript(omic_signature = make_lincs_sig(), file = path, verbose = FALSE))

  expect_false(out$visible)
  expect_equal(normalizePath(out$value), normalizePath(path))
})

test_that("a feature scored in both directions is left out of both", {
  tbl <- lincs_sig_table()
  tbl$feature_name[24] <- tbl$feature_name[1]
  path <- local_script_path()
  SigRepo::writeSignatureSearchScript(omic_signature = make_lincs_sig(tbl), file = path, verbose = FALSE)

  values <- script_values(path)
  expect_false(tbl$feature_name[1] %in% c(values$up_ids, values$down_ids))
  expect_length(values$up_ids, 11)
  expect_length(values$down_ids, 11)
})

test_that("features with a zero or missing score are left out", {
  tbl <- lincs_sig_table(n_up = 13, n_down = 13)
  tbl$score[13] <- 0
  tbl$score[26] <- NA
  path <- local_script_path()
  SigRepo::writeSignatureSearchScript(omic_signature = make_lincs_sig(tbl), file = path, verbose = FALSE)

  values <- script_values(path)
  expect_false(any(tbl$feature_name[c(13, 26)] %in% c(values$up_ids, values$down_ids)))
})

test_that("Ensembl IDs lose their version suffix and are mapped as ENSEMBL", {
  tbl <- lincs_sig_table()
  tbl$feature_name <- paste0(tbl$feature_name, ".", seq_len(nrow(tbl)))
  path <- local_script_path()
  SigRepo::writeSignatureSearchScript(omic_signature = make_lincs_sig(tbl), file = path, verbose = FALSE)

  values <- script_values(path)
  expect_equal(values$id_type, "ENSEMBL")
  expect_equal(values$up_ids, sprintf("ENSG%011d", 12:1))
})

test_that("feature names that are not Ensembl IDs are mapped as SYMBOL", {
  tbl <- lincs_sig_table()
  tbl$feature_name <- paste0("GENE", seq_len(nrow(tbl)))
  path <- local_script_path()
  SigRepo::writeSignatureSearchScript(omic_signature = make_lincs_sig(tbl), file = path, verbose = FALSE)

  values <- script_values(path)
  expect_equal(values$id_type, "SYMBOL")
  expect_equal(values$up_ids, paste0("GENE", 12:1))
})

test_that("n_genes is written into the script", {
  path <- local_script_path()
  SigRepo::writeSignatureSearchScript(omic_signature = make_lincs_sig(), file = path, n_genes = 50, verbose = FALSE)

  expect_equal(script_values(path)$n_genes, 50)
})

test_that("a signature name with quotes and line breaks cannot alter the script", {
  tbl <- lincs_sig_table()
  name <- "evil\"); stop('injected')\n stop('injected') #"
  sig <- make_hyper_sig(name = name, signature = tbl)
  path <- local_script_path()
  SigRepo::writeSignatureSearchScript(omic_signature = sig, file = path, verbose = FALSE)

  expect_equal(script_values(path)$signature_name, name)
  expect_false("injected" %in% unlist(lapply(parse(path), function(e) if (is.call(e) && identical(e[[1]], as.name("stop"))) as.character(e[[2]]))))
})

test_that("the organism check ignores case", {
  path <- local_script_path()
  # OmicSignature warns that this spelling is not in its organism list; the
  # bundled LLFS_Aging_Gene_2023 is stored with it.
  sig <- suppressWarnings(make_lincs_sig(organism = "Homo Sapiens"))

  expect_no_error(SigRepo::writeSignatureSearchScript(omic_signature = sig, file = path, verbose = FALSE))
})

test_that("a signature that is not human is refused", {
  path <- local_script_path()
  sig <- make_lincs_sig(organism = "Mus musculus")

  expect_error(
    SigRepo::writeSignatureSearchScript(omic_signature = sig, file = path, verbose = FALSE),
    "Homo sapiens.*Mus musculus"
  )
  expect_false(file.exists(path))
})

test_that("a signature that is not transcriptomics is refused", {
  path <- local_script_path()
  sig <- make_lincs_sig(assay_type = "proteomics")

  expect_error(
    SigRepo::writeSignatureSearchScript(omic_signature = sig, file = path, verbose = FALSE),
    "transcriptomics.*proteomics"
  )
  expect_false(file.exists(path))
})

test_that("fewer than 10 features in a direction is refused", {
  path <- local_script_path()
  sig <- make_lincs_sig(lincs_sig_table(n_up = 12, n_down = 9))

  expect_error(
    SigRepo::writeSignatureSearchScript(omic_signature = sig, file = path, verbose = FALSE),
    "12 up and 9 down"
  )
  expect_false(file.exists(path))
})

test_that("an existing file is kept unless overwrite = TRUE", {
  path <- local_script_path()
  writeLines("keep me", path)

  expect_error(
    SigRepo::writeSignatureSearchScript(omic_signature = make_lincs_sig(), file = path, verbose = FALSE),
    "overwrite = TRUE"
  )
  expect_equal(readLines(path), "keep me")

  SigRepo::writeSignatureSearchScript(omic_signature = make_lincs_sig(), file = path, overwrite = TRUE, verbose = FALSE)
  expect_equal(script_values(path)$signature_name, "lincs_sig")
})

test_that("file is required", {
  expect_error(
    SigRepo::writeSignatureSearchScript(omic_signature = make_lincs_sig(), verbose = FALSE),
    "'file' is required"
  )
})

test_that("n_genes below the 10-feature floor is refused", {
  path <- local_script_path()

  expect_error(
    SigRepo::writeSignatureSearchScript(omic_signature = make_lincs_sig(), file = path, n_genes = 5, verbose = FALSE),
    "'n_genes'"
  )
})
