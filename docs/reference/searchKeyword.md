# searchKeyword

Search for platform in the database

## Usage

``` r
searchKeyword(conn_handler, keyword = NULL, verbose = TRUE)
```

## Arguments

- conn_handler:

  An R object obtained from SigRepo::newConnhandler() (required)

- keyword:

  A list of keywords to be looked up. Default is NULL which will return
  all of the keywords in the database.

- verbose:

  Logical; whether or not to print the diagnostic messages. Default to
  'TRUE'.

## Examples
