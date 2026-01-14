# searchSampleType

Search sample types in the database

## Usage

``` r
searchSampleType(
  conn_handler,
  sample_type = NULL,
  brenda_accession = NULL,
  verbose = TRUE
)
```

## Arguments

- conn_handler:

  An R object obtained from SigRepo::newConnhandler() (required)

- sample_type:

  A sample type, or a list of sample types to search by. Default to
  'NULL', which will return all of the sample types in the database.

- brenda_accession:

  A list of Brenda accession to search by. Defaults to 'NULL'.

- verbose:

  Logical; whether or not to print the diagnostic messages. Defaults to
  'TRUE'.

## Examples
