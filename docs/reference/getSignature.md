# getSignature

Get a list of signatures uploaded by a specified user in the database.

## Usage

``` r
getSignature(
  conn_handler,
  signature_name = NULL,
  signature_id = NULL,
  verbose = TRUE
)
```

## Arguments

- conn_handler:

  An R object obtained from SigRepo::newConnhandler() (required)

- signature_name:

  Name of signature to be returned (required)

- signature_id:

  Database ID of signature to be returned (required)

- verbose:

  Logical; whether or not to print the diagnostic messages. Default is
  `TRUE`.

## Examples

``` r
if (FALSE) { # \dontrun{
SigRepo::getSignature(conn_handler = conn_handler,
                      signature_id = 56,
                      signature_name = "test_signature",
                      verbose = TRUE
)
} # }
      
```
