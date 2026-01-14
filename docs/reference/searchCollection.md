# searchCollection

Search for a list of collection in the database

## Usage

``` r
searchCollection(
  conn_handler,
  collection_id = NULL,
  collection_name = NULL,
  signature_name = NULL,
  signature_id = NULL,
  user_name = NULL,
  verbose = TRUE
)
```

## Arguments

- conn_handler:

  An R object obtained from SigRepo::newConnhandler() (required)

- collection_id:

  Database ID of collection to be looked up by.

- collection_name:

  Name of collection to be looked up by.

- signature_name:

  Signature name of the signatures to be looked up by.

- signature_id:

  Database ID of the signatures to be looked up by.

- user_name:

  Name of users that the collection belongs to.

- verbose:

  Logical; whether or not to print the diagnostic messages. Defaults to
  'TRUE'.

## Examples
