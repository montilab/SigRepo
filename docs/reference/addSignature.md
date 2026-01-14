# addSignature

Add signature to database

## Usage

``` r
addSignature(
  conn_handler,
  omic_signature,
  visibility = TRUE,
  add_users = NULL,
  return_signature_id = FALSE,
  return_missing_features = FALSE,
  verbose = TRUE
)
```

## Arguments

- conn_handler:

  An R object obtained from SigRepo::newConnhandler() (required)

- omic_signature:

  An OmicSignature R6 object from the OmicSignature package (required).

- visibility:

  Logical; whether the uploaded collection should be visible and
  accessible to others, Defaults to 'FALSE'

- add_users:

  A Data Frame; must contain the following column names: 'user_name',
  'access'. Access types are owner, viewer, or editor.This argument is
  only relevant when visibility is set to 'FALSE'.

- return_signature_id:

  Logical; if 'TRUE', the function will return the ID generated for the
  newly uploaded signature. Defaults to 'FALSE'.

- return_missing_features:

  Logical; if set to 'TRUE' the function will return a list of missing
  features present in the OmicSignature. Defaults to 'FALSE'

- verbose:

  Logical; whether to print diagnostic messages. Defaults to 'TRUE'

## Examples
