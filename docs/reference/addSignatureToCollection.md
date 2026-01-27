# addSignatureToCollection

Add a list of signatures to a collection in the database

## Usage

``` r
addSignatureToCollection(
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

  Database ID of the OmicSignature Collection (required)

- signature_id:

  A single signature database ID, or a list, to be added to a collection
  in the database (required)

- verbose:

  Logical; whether to print diagnostic messages. Defaults to 'TRUE'

## Examples
