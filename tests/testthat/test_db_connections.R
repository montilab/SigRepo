# testing scripts for the signature collection functions in the SigRepo package


# test connection handler
test_that("newConnHandler creates a connection handler correctly",{
  
  test_conn <- SigRepo::test_conn_handler
  
  # Expect test_conn is a list object with 7 elements
  expect_true(base::is.list(test_conn))
  expect_true(base::length(test_conn) == 7)
  expect_true(base::all(c("dbname", "host", "port", "user", "password", "api_host", "api_port") %in% base::names(test_conn)))
  
})

test_that("init_handle stores handle and get_handle returns it", {
  original_handle <- base::tryCatch(SigRepo::get_handle(), error = function(e) NULL)
  base::on.exit({
    if (base::is.null(original_handle)) {
      base::try(SigRepo::clear_handle(), silent = TRUE)
    } else {
      SigRepo::init_handle(original_handle)
    }
  }, add = TRUE)
  
  handle <- base::list(
    dbname = "sigrepo",
    host = "sigrepo.org",
    port = 3306,
    user = "tester",
    password = "secret",
    api_host = "sigrepo.org",
    api_port = 8020
  )
  
  expect_silent(SigRepo::init_handle(handle))
  expect_identical(SigRepo::get_handle(), handle)
})

test_that("init_handle rejects NULL handle", {
  expect_error(
    SigRepo::init_handle(NULL),
    "Cannot initialize handle with NULL"
  )
})

test_that("get_handle errors when no active handle exists", {
  original_handle <- base::tryCatch(SigRepo::get_handle(), error = function(e) NULL)
  base::on.exit({
    if (base::is.null(original_handle)) {
      base::try(SigRepo::clear_handle(), silent = TRUE)
    } else {
      SigRepo::init_handle(original_handle)
    }
  }, add = TRUE)
  
  base::try(SigRepo::clear_handle(), silent = TRUE)
  
  expect_error(
    SigRepo::get_handle(),
    "No active handle for the database connection found"
  )
})

test_that("clear_handle clears active handle and errors when none exists", {
  original_handle <- base::tryCatch(SigRepo::get_handle(), error = function(e) NULL)
  base::on.exit({
    if (base::is.null(original_handle)) {
      base::try(SigRepo::clear_handle(), silent = TRUE)
    } else {
      SigRepo::init_handle(original_handle)
    }
  }, add = TRUE)
  
  handle <- base::list(
    dbname = "sigrepo",
    host = "sigrepo.org",
    port = 3306,
    user = "tester",
    password = "secret",
    api_host = "sigrepo.org",
    api_port = 8020
  )
  
  SigRepo::init_handle(handle)
  expect_invisible(SigRepo::clear_handle())
  expect_error(
    SigRepo::get_handle(),
    "No active handle for the database connection found"
  )
  expect_error(
    SigRepo::clear_handle(),
    "No active handle to clear"
  )
})

test_that("conn_init uses explicitly supplied conn_handler instead of stored handle", {
  original_handle <- base::tryCatch(SigRepo::get_handle(), error = function(e) NULL)
  base::on.exit({
    if (base::is.null(original_handle)) {
      base::try(SigRepo::clear_handle(), silent = TRUE)
    } else {
      SigRepo::init_handle(original_handle)
    }
  }, add = TRUE)
  
  env_handle <- base::list(
    dbname = "sigrepo",
    host = "127.0.0.1",
    port = 3306,
    user = "viewer_user",
    password = "viewer_pw",
    api_host = "127.0.0.1",
    api_port = 8020
  )
  
  # Simulate a Shiny startup/root handler that differs from env state.
  root_handler <- base::list(
    dbname = "sigrepo",
    host = "127.0.0.1",
    port = 1,
    user = "root",
    password = "root_pw",
    api_host = "127.0.0.1",
    api_port = 8020
  )
  
  SigRepo::init_handle(env_handle)
  expect_identical(SigRepo::get_handle(), env_handle)
  
  expect_error(
    SigRepo::conn_init(conn_handler = root_handler),
    "Failed to connect to the database"
  )
  
  # conn_init() should have re-bound internal handle to supplied argument.
  expect_identical(SigRepo::get_handle(), root_handler)
})
