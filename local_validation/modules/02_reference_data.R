run_reference_data_validation <- function(ctx) {
  conn <- open_db_connection(get_sql_validation_handler(ctx))
  on.exit(close_db_connection(conn), add = TRUE)

  core_reference_tables <- c(
    "users",
    "organisms",
    "phenotypes",
    "sample_types",
    "platforms"
  )

  for (table_name in core_reference_tables) {
    n_rows <- table_count(conn, table_name)
    assert_true(
      ctx,
      n_rows > 0,
      sprintf("reference table populated: %s (%s rows)", table_name, n_rows),
      sprintf("reference table is empty: %s", table_name)
    )
  }

  if (isTRUE(ctx$config$expect_metabolite_reference)) {
    for (table_name in c("metabolite_reference", "metabolite_xref")) {
      n_rows <- table_count(conn, table_name)
      assert_true(
        ctx,
        n_rows > 0,
        sprintf("metabolomics reference table populated: %s (%s rows)", table_name, n_rows),
        sprintf("metabolomics reference table is empty: %s", table_name)
      )
    }
  } else {
    record_skip(ctx, "metabolite reference population check not requested")
  }

  if (isTRUE(ctx$config$expect_genesets)) {
    for (table_name in c("geneset_resources", "geneset_entries")) {
      n_rows <- table_count(conn, table_name)
      assert_true(
        ctx,
        n_rows > 0,
        sprintf("geneset table populated: %s (%s rows)", table_name, n_rows),
        sprintf("geneset table is empty: %s", table_name)
      )
    }
  } else {
    record_skip(ctx, "geneset population check not requested")
  }
}
