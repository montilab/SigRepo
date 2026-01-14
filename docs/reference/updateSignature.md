# updateSignature

Update a signature in the database

## Usage

``` r
updateSignature(
  conn_handler,
  signature_id,
  omic_signature,
  visibility = NULL,
  verbose = TRUE
)
```

## Arguments

- conn_handler:

  An R object obtained from SigRepo::newConnhandler() (required)

- signature_id:

  Database ID of signature to be updated (required)

- omic_signature:

  An R6 class object from the OmicSignature package (required)

- visibility:

  A logical value indicates whether or not to allow others to view and
  access one's uploaded signature. Defaults to 'FALSE'.

- verbose:

  Logical; whether or not to print the diagnostic messages. Defaults to
  'TRUE'.

## Examples
