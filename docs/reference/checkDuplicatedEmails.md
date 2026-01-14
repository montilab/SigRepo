# checkDuplicatedEmails

Check if provided user table has duplicated emails.

## Usage

``` r
checkDuplicatedEmails(
  conn,
  db_table_name = "users",
  table,
  coln_var = "user_email",
  check_db_table = TRUE
)
```

## Arguments

- conn:

  An established connection to database using newConnhandler()

- db_table_name:

  Name of a table in the database

- table:

  A data frame object

- coln_var:

  A list of column names to be excluded from the check.

- check_db_table:

  Check whether table exists in the database. Default = TRUE.
