# testing scripts for metabolomics support in collection upload

test_that("addCollection forwards metabolomics_nomenclature to addSignature", {
  expect_true("metabolomics_nomenclature" %in% names(formals(addCollection)))
  expect_null(formals(addCollection)$metabolomics_nomenclature)

  fn_body <- paste(deparse(body(addCollection)), collapse = "\n")

  expect_match(
    fn_body,
    "metabolomics_nomenclature = sig_metabolomics_nomenclature"
  )
})
