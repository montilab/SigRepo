# deleteSignature

Delete a signature from the signatures table of the database

## Usage

``` r
deleteSignature(conn_handler, signature_id = NULL, verbose = TRUE)
```

## Arguments

- conn_handler:

  An R object obtained from SigRepo::newConnhandler() (required)

- signature_id:

  Database ID of signature to be removed from the database (required)

- verbose:

  Logical; whether or not to print the diagnostic messages. Default is
  `TRUE`.

## Examples
