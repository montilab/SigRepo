# addCollection

Add signature collections to database

## Usage

``` r
addCollection(
  conn_handler,
  omic_collection,
  visibility = FALSE,
  return_collection_id = FALSE,
  verbose = TRUE
)
```

## Arguments

- conn_handler:

  An R object obtained from SigRepo::newConnhandler() (required)

- omic_collection:

  A collection of OmicSignature R6 objects from the OmicSignature
  package (required)

- visibility:

  Logical; whether the uploaded collection should be visible and
  accessible to others, Defaults to 'FALSE'

- return_collection_id:

  Logical; whether to return the ID of the uploaded collection Defaults
  to 'FALSE'

- verbose:

  Logical; whether to print diagnostic messages. Defaults to 'TRUE'.

## Examples
