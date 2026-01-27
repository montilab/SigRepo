# createOmicSignature

Get the signature set uploaded by a specific user in the database.

## Usage

``` r
createOmicSignature(conn_handler, db_signature_tbl)
```

## Arguments

- conn_handler:

  An R object obtained from SigRepo::newConnhandler() (required)

- db_signature_tbl:

  A Data Frame; must contain the following column names: 'signature_id'
