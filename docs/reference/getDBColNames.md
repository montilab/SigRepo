# getDBColNames

Get column names of a particular table in the database

## Usage

``` r
getDBColNames(
  conn,
  db_table_name,
  check_db_table = TRUE,
  exclude_coln_names = NULL
)
```

## Arguments

- conn:

  An established connection to database using SigRepo::newConnhandler()

- db_table_name:

  Name of a table in the database

- check_db_table:

  Check whether table exists in the database. Default = TRUE

- exclude_coln_names:

  optional flag to exclude column names from the Colnames list.
