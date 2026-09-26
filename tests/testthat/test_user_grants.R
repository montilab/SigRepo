# Offline checks for the GRANT statements addUser()/updateUser() issue, and for
# checkDBTable() reading SHOW TABLES. Both used to hard-code the `sigrepo`
# schema name, which broke seeding on any database with another name (#245,
# montilab/SigRepo_Server#130).

test_that("user_grant_statements names the connected database, never a literal sigrepo (#245)", {
  statements <- SigRepo::user_grant_statements(user_name = "montilab", user_role = "editor", dbname = "sigrepo_test")

  expect_true(length(statements) > 1)
  expect_true(all(grepl("`sigrepo_test`.", statements, fixed = TRUE)))
  expect_false(any(grepl("`sigrepo`.", statements, fixed = TRUE)))
})

test_that("editor grants include the table-level users grant in the connected database (#245)", {
  statements <- SigRepo::user_grant_statements(user_name = "montilab", user_role = "editor", dbname = "sigrepo_test")

  expect_true("GRANT SELECT ON `sigrepo_test`.`users` TO 'montilab'@'%';" %in% statements)
  expect_true("GRANT SELECT ON `sigrepo_test`.* TO 'montilab'@'%';" %in% statements)
})

test_that("admin and viewer grants are database-level only (#245)", {
  admin <- SigRepo::user_grant_statements(user_name = "boss", user_role = "admin", dbname = "other_db")
  viewer <- SigRepo::user_grant_statements(user_name = "guest", user_role = "viewer", dbname = "other_db")

  expect_equal(admin, "GRANT ALL PRIVILEGES ON `other_db`.* TO 'boss'@'%' WITH GRANT OPTION;")
  expect_equal(viewer, "GRANT SELECT ON `other_db`.* TO 'guest'@'%';")
})

test_that("checkDBTable reads the first SHOW TABLES column whatever the database is called (#245)", {
  testthat::local_mocked_bindings(
    dbGetQuery = function(conn, statement, ...) base::data.frame(Tables_in_other_db = c("users", "organisms"), stringsAsFactors = FALSE),
    dbDisconnect = function(conn, ...) TRUE,
    .package = "DBI"
  )

  expect_silent(SigRepo::checkDBTable(conn = NULL, db_table_name = "users"))
  expect_error(SigRepo::checkDBTable(conn = NULL, db_table_name = "phenotypes"), "There is no 'phenotypes' table")
})
