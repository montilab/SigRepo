# addSampleType

Add sample types to database

## Usage

``` r
addSampleType(conn_handler, sample_type_tbl, verbose = TRUE)
```

## Arguments

- conn_handler:

  An R object obtained from SigRepo::newConnhandler() (required)

- sample_type_tbl:

  A Data Frame; must contain the following column names: sample_type,
  brenda_accession (required)

- verbose:

  Logical; whether to print diagnostic messages. Defaults to 'TRUE'

## Examples
