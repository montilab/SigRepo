# getCollection

Get a list of collection uploaded by a specified user in the database.

## Usage

``` r
getCollection(
  conn_handler,
  collection_name = NULL,
  collection_id = NULL,
  verbose = TRUE
)
```

## Arguments

- conn_handler:

  An R object obtained from SigRepo::newConnhandler() (required)

- collection_name:

  Name of collection to be returned (required)

- collection_id:

  Database ID of collection to be returned (required)

- verbose:

  Logical; whether or not to print the diagnostic messages. Default is
  `TRUE`.
