run_signature_crud_validation <- function(ctx) {
  if (is.null(ctx$write_handler)) {
    record_skip(ctx, "signature CRUD skipped because write credentials are not configured")
    return(invisible(NULL))
  }

  assert_sigrepo_write_user(ctx)

  run_one_signature_crud <- function(assay_type, spec) {
    fixture <- load_fixture_object(spec)
    if (is.null(fixture)) {
      record_skip(ctx, sprintf("%s CRUD skipped because no fixture is configured", assay_type))
      return(invisible(NULL))
    }

    add_sig <- rebuild_signature_with_name(fixture, unique_local_name(paste0("local_", assay_type)))
    signature_id <- NULL

    add_args <- list(
      conn_handler = ctx$write_handler,
      omic_signature = add_sig,
      visibility = FALSE,
      return_signature_id = TRUE,
      verbose = FALSE
    )

    if (!is.null(spec$nomenclature) && nzchar(spec$nomenclature)) {
      add_args$metabolomics_nomenclature <- spec$nomenclature
    }

    signature_id <- do.call(SigRepo::addSignature, add_args)
    assert_true(
      ctx,
      length(signature_id) == 1 && !is.na(signature_id),
      sprintf("%s addSignature created signature_id=%s", assay_type, signature_id),
      sprintf("%s addSignature did not return a valid signature_id", assay_type)
    )

    on.exit(
      try(SigRepo::deleteSignature(
        conn_handler = ctx$write_handler,
        signature_id = signature_id,
        verbose = FALSE
      ), silent = TRUE),
      add = TRUE
    )

    retrieved <- SigRepo::getSignature(
      conn_handler = ctx$write_handler,
      signature_id = signature_id,
      verbose = FALSE
    )
    assert_true(
      ctx,
      is.list(retrieved) && length(retrieved) == 1,
      sprintf("%s getSignature returned the new signature", assay_type),
      sprintf("%s getSignature failed after addSignature", assay_type)
    )

    updated_sig <- rebuild_signature_with_name(
      retrieved[[1]],
      unique_local_name(paste0("updated_", assay_type))
    )

    update_args <- list(
      conn_handler = ctx$write_handler,
      signature_id = signature_id,
      omic_signature = updated_sig,
      verbose = FALSE
    )

    if (!is.null(spec$nomenclature) && nzchar(spec$nomenclature)) {
      update_args$metabolomics_nomenclature <- spec$nomenclature
    }

    do.call(SigRepo::updateSignature, update_args)

    updated_get <- SigRepo::getSignature(
      conn_handler = ctx$write_handler,
      signature_id = signature_id,
      verbose = FALSE
    )
    assert_true(
      ctx,
      identical(updated_get[[1]]$metadata$signature_name[[1]], updated_sig$metadata$signature_name[[1]]),
      sprintf("%s updateSignature changed signature_name", assay_type),
      sprintf("%s updateSignature did not persist the new signature_name", assay_type)
    )

    SigRepo::deleteSignature(
      conn_handler = ctx$write_handler,
      signature_id = signature_id,
      verbose = FALSE
    )

    deleted_get <- SigRepo::getSignature(
      conn_handler = ctx$write_handler,
      signature_id = signature_id,
      verbose = FALSE
    )
    assert_true(
      ctx,
      is.null(deleted_get),
      sprintf("%s deleteSignature removed the signature", assay_type),
      sprintf("%s deleteSignature did not remove the signature", assay_type)
    )
  }

  for (assay_type in names(ctx$fixture_specs)) {
    if (assay_type == "methylomics") {
      record_skip(ctx, "methylomics CRUD fixture support is scaffolded but not enabled by default")
      next
    }
    run_one_signature_crud(assay_type, ctx$fixture_specs[[assay_type]])
  }
}
