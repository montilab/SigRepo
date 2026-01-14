# removeDuplicates

Remove duplicates from table in the database

## Usage

``` r
removeDuplicates(conn, db_table_name, table, coln_var, check_db_table = TRUE)
```

## Arguments

- conn:

  An established connection to database using SigRepo::newConnhandler()

- db_table_name:

  Name of a table in the database

- table:

  A data frame object

- coln_var:

  A column variable in the data table

- check_db_table:

  whether to check database table. Default = TRUE
