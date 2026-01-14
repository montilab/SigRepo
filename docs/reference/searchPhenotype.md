# searchPhenotype

Search for a phenotype in the database

## Usage

``` r
searchPhenotype(conn_handler, phenotype = NULL, verbose = TRUE)
```

## Arguments

- conn_handler:

  An R object obtained from SigRepo::newConnhandler() (required)

- phenotype:

  A phenotype, or a list of phenotypes to search by. Default is NULL
  which will return all of the phenotypes in the database

- verbose:

  Logical; whether or not to print the diagnostic messages. Defaults to
  'TRUE'.

## Examples
