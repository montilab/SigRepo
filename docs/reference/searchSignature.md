# searchSignature

Search for a list of signatures in the database

## Usage

``` r
searchSignature(
  conn_handler,
  signature_id = NULL,
  signature_name = NULL,
  user_name = NULL,
  organism = NULL,
  phenotype = NULL,
  sample_type = NULL,
  platform_name = NULL,
  verbose = TRUE
)
```

## Arguments

- conn_handler:

  An R object obtained from SigRepo::newConnhandler() (required)

- signature_id:

  Database ID of the signatures to be looked up by.

- signature_name:

  Name of the signatures to be looked up by.

- user_name:

  The name of the user to be looked up by.

- organism:

  The organism to be looked up by.

- phenotype:

  The phenotype to be looked up by.

- sample_type:

  The sample type to be looked up by.

- platform_name:

  The platform name to be looked up by.

- verbose:

  Logical; whether or not to print the diagnostic messages. Defaults to
  'TRUE'.

## Examples
