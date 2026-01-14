# updateProteomicsFeatureSet

Retrieve FTP UniProt data from NCBI and update proteomics feature set in
the database

## Usage

``` r
updateProteomicsFeatureSet(conn_handler, organism, verbose = TRUE)
```

## Arguments

- conn_handler:

  An R object obtained from SigRepo::newConnhandler() (required)

- organism:

  An organism to extract and update its features (required)

- verbose:

  A logical value of whether to print diagnostic messages. Defaults to
  'TRUE'
