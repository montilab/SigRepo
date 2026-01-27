# checkTableInput

Check table against a particular table in the database.

## Usage

``` r
checkTableInput(
  conn,
  db_table_name,
  table,
  exclude_coln_names = NULL,
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

- exclude_coln_names:

  A list of column names to be excluded from the check.

- check_db_table:

  Check whether table exists in the database. Default = TRUE.
