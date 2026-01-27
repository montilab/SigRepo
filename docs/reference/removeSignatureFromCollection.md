# removeSignatureFromCollection

Delete a list of signatures from a collection in the database

## Usage

``` r
removeSignatureFromCollection(
  conn_handler,
  collection_id,
  signature_id,
  verbose = TRUE
)
```

## Arguments

- conn_handler:

  An R object obtained from SigRepo::newConnhandler() (required)

- collection_id:

  Database ID of the collection (required)

- signature_id:

  A list of signature database IDs to be removed from a collection in
  the database (required)

- verbose:

  Logical; whether or not to print the diagnostic messages. Default is
  `TRUE`.

## Examples
