# addTranscriptomicsSignatureSet

Add a signature feature set to database

## Usage

``` r
addTranscriptomicsSignatureSet(
  conn_handler,
  signature_id,
  organism_id,
  signature_set,
  verbose = TRUE
)
```

## Arguments

- conn_handler:

  An R object obtained from SigRepo::newConnhandler() (required)

- signature_id:

  Database ID of the signature (required)

- organism_id:

  Database ID of the organism (required)

- signature_set:

  A Data Frame; Must contain the following column names: feature_name,
  probe_id, score, group_label (required)

- verbose:

  Logical; whether to print diagnostic messages. Defaults to 'TRUE'.
