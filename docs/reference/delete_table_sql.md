# delete_table_sql

delete an entry from database table

## Usage

``` r
delete_table_sql(
  conn,
  db_table_name,
  delete_coln_var,
  delete_coln_val,
  check_db_table = TRUE
)
```

## Arguments

- conn:

  An established database connection

- db_table_name:

  Name of a table in the database

- delete_coln_var:

  A column variable in the table for removing rows

- delete_coln_val:

  A list of values associated with delete_coln_var to be removed.

- check_db_table:

  whether to check database table. Default = TRUE.
