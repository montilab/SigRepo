# searchGeneticVariantsFeatureSet

Search for a list of GeneticVariants features in the database

## Usage

``` r
searchGeneticVariantsFeatureSet(
  conn_handler,
  feature_name = NULL,
  organism = NULL,
  verbose = TRUE
)
```

## Arguments

- conn_handler:

  An R object obtained from SigRepo::newConnhandler() (required)

- feature_name:

  A list of feature names to look up. Defaults to 'NULL', which will
  return all of the feature names in the database.

- organism:

  A list of organism to look up. Default to 'NULL' which will return all
  of the organisms in the database.

- verbose:

  Logical; whether or not to print the diagnostic messages. Defaults to
  'TRUE'.

## Examples

``` r
if (FALSE) { # \dontrun{
SigRepo::searchGeneticVariantsFeatureSet(conn_handler = conn_handler,
                                    feature_name = "test_feature",
                                    organism = "Homo sapiens"
                                    )
} # }

```
