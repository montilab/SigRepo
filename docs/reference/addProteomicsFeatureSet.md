# addProteomicsFeatureSet

Add proteomics feature set to database

## Usage

``` r
addProteomicsFeatureSet(conn_handler, feature_set, verbose = TRUE)
```

## Arguments

- conn_handler:

  An R object obtained from SigRepo::newConnhandler() (required)

- feature_set:

  A Data Frame; must contain the following column names: feature_name,
  organism, gene_symbol, is_current, version (required)

- verbose:

  Logical; whether to print diagnostic messages. Defaults to 'TRUE'
