# updateTranscriptomicsFeatureSet

Extract data from biomaRt package and update transcriptomics feature set
in the database.

## Usage

``` r
updateTranscriptomicsFeatureSet(conn_handler, organism, verbose = TRUE)
```

## Arguments

- conn_handler:

  An R object obtained from SigRepo::newConnhandler() (required)

- organism:

  An organism to extract and update its features (required)

- verbose:

  A logical value of whether to print diagnostic messages. Defaults to
  'TRUE'.
