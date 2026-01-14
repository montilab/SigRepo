# addSignatureWithID

Add signature to database with an assigned ID

## Usage

``` r
addSignatureWithID(
  conn_handler,
  omic_signature,
  assign_signature_id,
  assign_user_name,
  visibility = FALSE,
  check_difexp = TRUE,
  verbose = FALSE
)
```

## Arguments

- conn_handler:

  An R object obtained from SigRepo::newConnhandler() (required)

- omic_signature:

  An OmicSignature R6 object from the OmicSignature package (required).

- assign_signature_id:

  Assign an unique ID to the uploaded signature (required)

- assign_user_name:

  Assign an unique user name to the uploaded signature (required)

- visibility:

  Logical; whether the uploaded collection should be visible and
  accessible to others, Defaults to 'FALSE'

- check_difexp:

  Logical; whether or not to check difexp table in the database.
  Defaults to 'TRUE'

- verbose:

  Logical; whether to print diagnostic messages. Defaults to 'TRUE'
