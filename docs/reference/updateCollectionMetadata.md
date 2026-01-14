# updateCollectionMetadata

Update metadata of a collection in the database

## Usage

``` r
updateCollectionMetadata(
  conn_handler,
  collection_id,
  collection_name = NULL,
  description = NULL,
  visibility = NULL,
  verbose = TRUE
)
```

## Arguments

- conn_handler:

  An R object obtained from SigRepo::newConnhandler() (required)

- collection_id:

  Database ID of collection in the database to be updated (required)

- collection_name:

  Name of the collection to be changed.

- description:

  Description of the collection to be changed.

- visibility:

  Logical; whether or not to allow others to view and access one's
  uploaded collection. Defaults to 'TRUE'

- verbose:

  a logical value indicates whether or not to print the diagnostic
  messages. Default is `TRUE`.
