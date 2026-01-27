# createOmicCollection

Get the collection set uploaded by a specific user in the database.

## Usage

``` r
createOmicCollection(conn_handler, db_collection_tbl)
```

## Arguments

- conn_handler:

  An R object obtained from SigRepo::newConnhandler() (required)

- db_collection_tbl:

  A Data Frame; must have the following column name(s): 'collecion_id'.
