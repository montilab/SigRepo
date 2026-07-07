run_collection_crud_validation <- function(ctx) {
  if (is.null(ctx$write_handler)) {
    record_skip(ctx, "collection CRUD skipped because write credentials are not configured")
    return(invisible(NULL))
  }

  assert_sigrepo_write_user(ctx)

  data("omic_signature_1", package = "SigRepo", envir = environment())
  data("omic_signature_2", package = "SigRepo", envir = environment())

  sig_1 <- rebuild_signature_with_name(omic_signature_1, unique_local_name("collection_sig_1"))
  sig_2 <- rebuild_signature_with_name(omic_signature_2, unique_local_name("collection_sig_2"))

  collection_name <- unique_local_name("local_collection")
  collection_obj <- OmicSignature::OmicSignatureCollection$new(
    OmicSigList = list(sig_1 = sig_1, sig_2 = sig_2),
    metadata = list(
      collection_name = collection_name,
      description = "Local validation collection"
    )
  )

  collection_id <- SigRepo::addCollection(
    conn_handler = ctx$write_handler,
    omic_collection = collection_obj,
    visibility = FALSE,
    return_collection_id = TRUE,
    verbose = FALSE
  )

  assert_true(
    ctx,
    length(collection_id) == 1 && !is.na(collection_id),
    sprintf("addCollection created collection_id=%s", collection_id),
    "addCollection did not return a valid collection_id"
  )

  on.exit(
    try(SigRepo::deleteCollection(
      conn_handler = ctx$write_handler,
      collection_id = collection_id,
      verbose = FALSE
    ), silent = TRUE),
    add = TRUE
  )

  retrieved <- SigRepo::getCollection(
    conn_handler = ctx$write_handler,
    collection_id = collection_id,
    verbose = FALSE
  )
  assert_true(
    ctx,
    is.list(retrieved) && length(retrieved) == 1,
    "getCollection returned the new collection",
    "getCollection failed after addCollection"
  )

  updated_name <- unique_local_name("updated_collection")
  SigRepo::updateCollectionMetadata(
    conn_handler = ctx$write_handler,
    collection_id = collection_id,
    collection_name = updated_name,
    description = "Updated local validation collection",
    verbose = FALSE
  )

  updated <- SigRepo::getCollection(
    conn_handler = ctx$write_handler,
    collection_id = collection_id,
    verbose = FALSE
  )
  assert_true(
    ctx,
    identical(updated[[1]]$metadata$collection_name[[1]], updated_name),
    "updateCollectionMetadata changed collection_name",
    "updateCollectionMetadata did not persist collection_name"
  )

  SigRepo::deleteCollection(
    conn_handler = ctx$write_handler,
    collection_id = collection_id,
    verbose = FALSE
  )

  deleted <- tryCatch(
    SigRepo::getCollection(
      conn_handler = ctx$write_handler,
      collection_id = collection_id,
      verbose = FALSE
    ),
    error = function(e) NULL
  )
  assert_true(
    ctx,
    is.null(deleted),
    "deleteCollection removed the collection",
    "deleteCollection did not remove the collection"
  )
}
