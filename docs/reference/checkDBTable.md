# checkDBTable

Check api key whether it is valid to access the database

## Usage

``` r
checkDBTable(conn, db_table_name, check = TRUE)
```

## Arguments

- conn:

  An established connection to database using SigRepo::newConnhandler()

- db_table_name:

  Name of table in the database

- check:

  Check whether table exists in the database. Default = TRUE.
