# testing script for platform functions in the database
# addPlatform
# searchPlatform

test_that("addPlatform correctly adds the platform into the database", {
  test_conn <- create_test_conn()
  
  # Create platform data for this test
  platform_table <- base::data.frame(
    platform_name = "test_platform"
  )
  
  expect_no_error({
    SigRepo::addPlatform(
      conn_handler = test_conn,
      platform_tbl = platform_table,
      verbose = TRUE
    )
  })
})

test_that("searchPlatform correctly searches for the desired platform", {
  test_conn <- create_test_conn()
  
  platform_search <- SigRepo::searchPlatform(
    conn_handler = test_conn,
    platform = 'test_platform',
    verbose = TRUE
  )
  
  expect_true(methods::is(platform_search, "data.frame"))
  expect_true(nrow(platform_search) > 0)
  expect_equal(platform_search$platform[1], "test_platform")
})

test_that("addPlatform handles duplicate platforms", {
  test_conn <- create_test_conn()
  
  # Create platform data for this test
  platform_table <- base::data.frame(
    platform_name = "duplicate_test_platform"
  )

  # Add platform first time
  SigRepo::addPlatform(
    conn_handler = test_conn,
    platform_tbl = platform_table,
    verbose = FALSE
  )
  
  # Try to add same platform again - should handle gracefully
  # Should it error or not?
  expect_no_error({
    SigRepo::addPlatform(
      conn_handler = test_conn,
      platform_tbl = platform_table,
      verbose = FALSE
    )
  }) 
  # expect_error({
  #   SigRepo::addPlatform(
  #     conn_handler = test_conn,
  #     platform_tbl = platform_table,
  #     verbose = FALSE
  #   )
  # })
})

test_that("searchPlatform returns empty result for non-existent platform", {
  test_conn <- create_test_conn()
  
  platform_search <- SigRepo::searchPlatform(
    conn_handler = test_conn,
    platform = 'non_existent_platform_xyz123',
    verbose = FALSE
  )
  
  expect_true(methods::is(platform_search, "data.frame"))
  expect_true(nrow(platform_search) == 0)
})

test_that("searchPlatform without specific platform returns all platforms", {
  test_conn <- create_test_conn()
  
  all_platforms <- SigRepo::searchPlatform(
    conn_handler = test_conn,
    verbose = FALSE
  )
  
  expect_true(methods::is(all_platforms, "data.frame"))
  expect_true(nrow(all_platforms) >= 0)
})

test_that("addPlatform validates input data frame", {
  test_conn <- create_test_conn()
  
  # Test with NULL
  expect_error({
    SigRepo::addPlatform(
      conn_handler = test_conn,
      platform_tbl = NULL,
      verbose = FALSE
    )
  })
  
  # Test with empty data frame
  expect_error({
    SigRepo::addPlatform(
      conn_handler = test_conn,
      platform_tbl = data.frame(),
      verbose = FALSE
    )
  })
  
  # # If error is expected
  # expect_no_error({
  #   SigRepo::addPlatform(
  #     conn_handler = test_conn,
  #     platform_tbl = data.frame(),
  #     verbose = FALSE
  #   )
  # })
})

test_that("searchPlatform handles NULL connection handler", {
  expect_error(
    SigRepo::searchPlatform(conn_handler = NULL, platform = "test")
  )
})

test_that("addPlatform handles NULL connection handler", {
  platform_table <- data.frame(platform = "test", platform_type = "test_type")
  
  expect_error(
    SigRepo::addPlatform(conn_handler = NULL, platform_tbl = platform_table)
  )
})