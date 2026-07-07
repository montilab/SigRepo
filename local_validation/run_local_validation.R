options(stringsAsFactors = FALSE)

script_dir <- normalizePath("local_validation")

source(file.path(script_dir, "modules", "helpers.R"))

env_file <- Sys.getenv(
  "SIGREPO_LOCAL_ENV_FILE",
  unset = file.path(script_dir, ".env.local-validation")
)
load_env_file(env_file)

module_files <- list.files(
  file.path(script_dir, "modules"),
  pattern = "^[0-9]{2}_.*\\.R$",
  full.names = TRUE
)

for (module_file in module_files) {
  source(module_file)
}

ctx <- build_local_validation_context(script_dir = script_dir)

for (module_name in names(ctx$modules)) {
  run_validation_module(ctx, module_name, ctx$modules[[module_name]])
}

print_validation_summary(ctx)

if (ctx$fail_count > 0) {
  quit(status = 1)
}
