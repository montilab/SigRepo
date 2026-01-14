# createSignatureMetadata

Create a metadata object for a signature

## Usage

``` r
createSignatureMetadata(conn_handler, omic_signature, verbose = FALSE)
```

## Arguments

- conn_handler:

  An R object obtained from SigRepo::newConnhandler() (required)

- omic_signature:

  An OmicSignature R6 object from the OmicSignature package (required).

- verbose:

  Logical; whether to print diagnostic messages. Defaults to 'TRUE'.
