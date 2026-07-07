`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

load_env_file <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    return(invisible(FALSE))
  }

  lines <- readLines(path, warn = FALSE)
  lines <- trimws(lines)
  lines <- lines[!lines %in% ""]
  lines <- lines[!grepl("^#", lines)]

  for (line in lines) {
    if (startsWith(line, "export ")) {
      line <- sub("^export\\s+", "", line)
    }

    if (!grepl("=", line, fixed = TRUE)) {
      next
    }

    key <- sub("=.*$", "", line)
    value <- sub("^[^=]*=", "", line)
    key <- trimws(key)
    value <- trimws(value)

    if ((startsWith(value, "\"") && endsWith(value, "\"")) ||
        (startsWith(value, "'") && endsWith(value, "'"))) {
      value <- substring(value, 2, nchar(value) - 1)
    }

    do.call(base::Sys.setenv, stats::setNames(list(value), key))
  }

  invisible(TRUE)
}

env_flag <- function(name, default = FALSE) {
  value <- Sys.getenv(name, unset = NA_character_)
  if (is.na(value) || !nzchar(value)) {
    return(default)
  }
  tolower(trimws(value)) %in% c("1", "true", "yes", "y")
}

env_value <- function(name, default = NULL) {
  value <- Sys.getenv(name, unset = NA_character_)
  if (is.na(value) || !nzchar(value)) default else value
}

log_line <- function(prefix, text) {
  cat(sprintf("%s %s\n", prefix, text))
}

build_conn_handler_from_config <- function(cfg, user, password) {
  SigRepo::newConnHandler(
    dbname = cfg$dbname,
    host = cfg$db_host,
    port = cfg$db_port,
    user = user,
    password = password,
    api_host = cfg$api_host,
    api_port = cfg$api_port
  )
}

build_local_validation_context <- function(script_dir) {
  env_file <- env_value(
    "SIGREPO_LOCAL_ENV_FILE",
    file.path(script_dir, ".env.local-validation")
  )
  load_env_file(env_file)

  if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(path = ".", quiet = TRUE, export_all = FALSE, helpers = FALSE)
  } else if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(path = ".", quiet = TRUE, export_all = FALSE, helpers = FALSE)
  } else {
    base::library("SigRepo", character.only = TRUE)
  }

  cfg <- list(
    script_dir = script_dir,
    env_file = env_file,
    dbname = env_value("SIGREPO_LOCAL_DB_NAME", "sigrepo"),
    db_host = env_value("SIGREPO_LOCAL_DB_HOST", "127.0.0.1"),
    db_port = as.numeric(env_value("SIGREPO_LOCAL_DB_PORT", "3306")),
    api_host = env_value("SIGREPO_LOCAL_API_HOST", "http://127.0.0.1"),
    api_port = as.numeric(env_value("SIGREPO_LOCAL_API_PORT", "8020")),
    db_admin_user = env_value("SIGREPO_LOCAL_DB_ADMIN_USER", ""),
    db_admin_password = env_value("SIGREPO_LOCAL_DB_ADMIN_PASSWORD", ""),
    read_user = env_value("SIGREPO_LOCAL_READ_USER", "guest"),
    read_password = env_value("SIGREPO_LOCAL_READ_PASSWORD", "guest"),
    write_user = env_value("SIGREPO_LOCAL_WRITE_USER", ""),
    write_password = env_value("SIGREPO_LOCAL_WRITE_PASSWORD", ""),
    expect_genesets = env_flag("SIGREPO_LOCAL_EXPECT_GENESETS", FALSE),
    expect_metabolite_reference = env_flag("SIGREPO_LOCAL_EXPECT_METABOLITE_REFERENCE", FALSE),
    proteomics_fixture = env_value("SIGREPO_LOCAL_PROTEOMICS_FIXTURE", NULL),
    metabolomics_fixture = env_value("SIGREPO_LOCAL_METABOLOMICS_FIXTURE", NULL),
    metabolomics_nomenclature = env_value("SIGREPO_LOCAL_METABOLOMICS_NOMENCLATURE", NULL),
    genetic_variants_fixture = env_value("SIGREPO_LOCAL_GENETIC_VARIANTS_FIXTURE", NULL),
    methylomics_fixture = env_value("SIGREPO_LOCAL_METHYLOMICS_FIXTURE", NULL)
  )

  read_handler <- build_conn_handler_from_config(cfg, cfg$read_user, cfg$read_password)

  db_admin_handler <- NULL
  if (nzchar(cfg$db_admin_user) && nzchar(cfg$db_admin_password)) {
    db_admin_handler <- build_conn_handler_from_config(cfg, cfg$db_admin_user, cfg$db_admin_password)
  }

  write_handler <- NULL
  if (nzchar(cfg$write_user) && nzchar(cfg$write_password)) {
    write_handler <- build_conn_handler_from_config(cfg, cfg$write_user, cfg$write_password)
  }

  fixture_specs <- list(
    transcriptomics = list(source = "builtin", object = "LLFS_Aging_Gene_2023", nomenclature = NULL),
    proteomics = list(source = cfg$proteomics_fixture, object = NULL, nomenclature = NULL),
    metabolomics = list(source = cfg$metabolomics_fixture, object = NULL, nomenclature = cfg$metabolomics_nomenclature),
    genetic_variants = list(source = cfg$genetic_variants_fixture, object = NULL, nomenclature = NULL),
    methylomics = list(source = cfg$methylomics_fixture, object = NULL, nomenclature = NULL)
  )

  modules <- list(
    db_schema = run_db_schema_validation,
    reference_data = run_reference_data_validation,
    r_client_read = run_r_client_read_validation,
    signature_crud = run_signature_crud_validation,
    collection_crud = run_collection_crud_validation
  )

  structure(list(
    config = cfg,
    db_admin_handler = db_admin_handler,
    read_handler = read_handler,
    write_handler = write_handler,
    fixture_specs = fixture_specs,
    modules = modules,
    pass_count = 0L,
    fail_count = 0L,
    skip_count = 0L
  ), class = "sigrepo_local_validation_context")
}

record_pass <- function(ctx, text) {
  ctx$pass_count <- ctx$pass_count + 1L
  log_line("[pass]", text)
}

record_fail <- function(ctx, text) {
  ctx$fail_count <- ctx$fail_count + 1L
  log_line("[fail]", text)
}

record_skip <- function(ctx, text) {
  ctx$skip_count <- ctx$skip_count + 1L
  log_line("[skip]", text)
}

assert_true <- function(ctx, condition, success, failure) {
  if (isTRUE(condition)) {
    record_pass(ctx, success)
  } else {
    stop(failure, call. = FALSE)
  }
}

run_validation_module <- function(ctx, module_name, module_fun) {
  log_line("[module]", module_name)
  tryCatch(
    module_fun(ctx),
    error = function(e) record_fail(ctx, sprintf("%s: %s", module_name, conditionMessage(e)))
  )
}

print_validation_summary <- function(ctx) {
  cat("\n")
  log_line("[summary]", sprintf("pass=%s fail=%s skip=%s", ctx$pass_count, ctx$fail_count, ctx$skip_count))
}

open_db_connection <- function(conn_handler) {
  SigRepo::conn_init(conn_handler)
}

get_sql_validation_handler <- function(ctx) {
  if (!is.null(ctx$db_admin_handler)) {
    return(ctx$db_admin_handler)
  }
  if (!is.null(ctx$write_handler)) {
    return(ctx$write_handler)
  }
  ctx$read_handler
}

close_db_connection <- function(conn) {
  suppressWarnings(DBI::dbDisconnect(conn))
}

table_exists <- function(conn, table_name) {
  all_tables <- DBI::dbGetQuery(conn, "show tables;")
  table_name %in% unlist(all_tables, use.names = FALSE)
}

table_count <- function(conn, table_name) {
  DBI::dbGetQuery(conn, sprintf("SELECT COUNT(*) AS n FROM %s", table_name))$n[[1]]
}

load_fixture_object <- function(spec) {
  if (identical(spec$source, "builtin")) {
    data(list = spec$object, package = "SigRepo", envir = environment())
    return(get(spec$object, inherits = FALSE))
  }

  if (is.null(spec$source) || !nzchar(spec$source)) {
    return(NULL)
  }

  readRDS(spec$source)
}

rebuild_signature_with_name <- function(omic_signature, signature_name) {
  metadata <- omic_signature$metadata
  metadata$signature_name <- signature_name
  OmicSignature::OmicSignature$new(
    metadata = metadata,
    signature = omic_signature$signature,
    difexp = omic_signature$difexp
  )
}

unique_local_name <- function(prefix) {
  paste0(prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", sample.int(99999, 1))
}

get_sigrepo_user_row <- function(conn_handler) {
  conn <- open_db_connection(conn_handler)
  on.exit(close_db_connection(conn), add = TRUE)

  conn_info <- DBI::dbGetInfo(conn)
  user_name <- conn_info$user

  user_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = "users",
    return_var = c("user_name", "user_role", "active", "api_key"),
    filter_coln_var = "user_name",
    filter_coln_val = base::list("user_name" = user_name),
    check_db_table = TRUE
  )

  if (nrow(user_tbl) == 0) {
    return(NULL)
  }

  user_tbl[1, , drop = FALSE]
}

assert_sigrepo_write_user <- function(ctx) {
  if (is.null(ctx$write_handler)) {
    record_skip(ctx, "write-path validation skipped because write credentials are not configured")
    return(NULL)
  }

  user_row <- get_sigrepo_user_row(ctx$write_handler)
  conn <- open_db_connection(ctx$write_handler)
  conn_info <- DBI::dbGetInfo(conn)
  close_db_connection(conn)

  if (is.null(user_row)) {
    stop(
      sprintf(
        "write user '%s' is a valid MySQL login but is not registered in the SigRepo 'users' table. Use SIGREPO_LOCAL_DB_ADMIN_USER for root/bootstrap tasks and SIGREPO_LOCAL_WRITE_USER for an active SigRepo admin/editor account.",
        conn_info$user
      ),
      call. = FALSE
    )
  }

  if (!isTRUE(as.integer(user_row$active[[1]]) == 1L)) {
    stop(
      sprintf("write user '%s' exists in SigRepo but is not active.", user_row$user_name[[1]]),
      call. = FALSE
    )
  }

  if (!user_row$user_role[[1]] %in% c("admin", "editor")) {
    stop(
      sprintf(
        "write user '%s' has SigRepo role '%s'. Local CRUD validation requires an active 'admin' or 'editor' user.",
        user_row$user_name[[1]],
        user_row$user_role[[1]]
      ),
      call. = FALSE
    )
  }

  user_row
}
