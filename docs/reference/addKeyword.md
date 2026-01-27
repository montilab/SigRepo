# addKeyword

Add keywords to database

## Usage

``` r
addKeyword(conn_handler, keyword_tbl, verbose = TRUE)
```

## Arguments

- conn_handler:

  An R object obtained from SigRepo::newConnhandler() (required)

- keyword_tbl:

  A Data Frame; Must contain the following column names: keyword
  (required)

- verbose:

  Logical; whether to print diagnostic messages. Defaults to 'TRUE'

## Examples
