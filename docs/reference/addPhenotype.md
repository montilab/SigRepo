# addPhenotype

Add phenotypes to database

## Usage

``` r
addPhenotype(conn_handler, phenotype_tbl, verbose = TRUE)
```

## Arguments

- conn_handler:

  An R object obtained from SigRepo::newConnhandler() (required)

- phenotype_tbl:

  A Data Frame; Must contain the following column names: phenotype
  (required)

- verbose:

  Logical; whether to print diagnostic messages. Defaults to 'TRUE'

## Examples
