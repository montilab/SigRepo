run_db_schema_validation <- function(ctx) {
  conn <- open_db_connection(get_sql_validation_handler(ctx))
  on.exit(close_db_connection(conn), add = TRUE)

  required_tables <- c(
    "users",
    "organisms",
    "phenotypes",
    "sample_types",
    "platforms",
    "signatures",
    "signature_feature_set",
    "signature_access",
    "collection",
    "collection_access",
    "signature_collection_access",
    "transcriptomics_features",
    "proteomics_features",
    "genetic_variants_features",
    "metabolite_reference",
    "metabolite_xref",
    "geneset_resources",
    "geneset_entries"
  )

  for (table_name in required_tables) {
    assert_true(
      ctx,
      table_exists(conn, table_name),
      sprintf("table exists: %s", table_name),
      sprintf("required table is missing: %s", table_name)
    )
  }
}
