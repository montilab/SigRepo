run_r_client_read_validation <- function(ctx) {
  assay_tbl <- SigRepo::searchAssayType(conn_handler = ctx$read_handler, verbose = FALSE)
  assert_true(
    ctx,
    is.data.frame(assay_tbl) && nrow(assay_tbl) > 0,
    sprintf("searchAssayType returned %s rows", nrow(assay_tbl)),
    "searchAssayType returned no rows"
  )

  organism_tbl <- SigRepo::searchOrganism(conn_handler = ctx$read_handler, verbose = FALSE)
  assert_true(
    ctx,
    is.data.frame(organism_tbl) && nrow(organism_tbl) > 0,
    sprintf("searchOrganism returned %s rows", nrow(organism_tbl)),
    "searchOrganism returned no rows"
  )

  signature_hits <- SigRepo::searchSignature(
    conn_handler = ctx$read_handler,
    signature_name = "LLFS_Aging_Gene_2023",
    verbose = FALSE
  )
  assert_true(
    ctx,
    is.data.frame(signature_hits) && nrow(signature_hits) >= 1,
    "searchSignature found LLFS_Aging_Gene_2023",
    "searchSignature did not find LLFS_Aging_Gene_2023"
  )

  omic_sigs <- SigRepo::getSignature(
    conn_handler = ctx$read_handler,
    signature_name = "LLFS_Aging_Gene_2023",
    verbose = FALSE
  )
  assert_true(
    ctx,
    is.list(omic_sigs) && length(omic_sigs) >= 1,
    "getSignature returned at least one OmicSignature",
    "getSignature returned no signatures"
  )
}
