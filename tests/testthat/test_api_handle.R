# testing scripts for API handle helpers in the SigRepo package

test_that("build_api_url uses stored handle when conn_handler is omitted", {
  init_handle(list(
    dbname = "sigrepo",
    host = "sigrepo.org",
    port = 3306,
    user = "guest",
    password = "guest",
    api_host = "api.example.org",
    api_port = 8020
  ))
  on.exit(try(clear_handle(), silent = TRUE), add = TRUE)

  api_url <- build_api_url(
    endpoint = "get_difexp",
    query = list(api_key = "abc123", signature_hashkey = "hash456")
  )

  expect_identical(
    api_url,
    "http://api.example.org:8020/get_difexp?api_key=abc123&signature_hashkey=hash456"
  )
})

test_that("build_api_url prefers an explicit conn_handler", {
  init_handle(list(
    api_host = "api.example.org",
    api_port = 8020
  ))
  on.exit(try(clear_handle(), silent = TRUE), add = TRUE)

  api_url <- build_api_url(
    conn_handler = list(api_host = "override.example.org", api_port = 9000),
    endpoint = "store_difexp",
    query = list(api_key = "key")
  )

  expect_identical(
    api_url,
    "http://override.example.org:9000/store_difexp?api_key=key"
  )
})

test_that("build_api_url preserves explicit scheme and existing port in api_host", {
  init_handle(list(
    api_host = "https://api.example.org:8443",
    api_port = 8020
  ))
  on.exit(try(clear_handle(), silent = TRUE), add = TRUE)

  api_url <- build_api_url(
    endpoint = "/get_difexp",
    query = list(api_key = "abc")
  )

  expect_identical(
    api_url,
    "https://api.example.org:8443/get_difexp?api_key=abc"
  )
})
