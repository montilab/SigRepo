# addPlatform

Add platforms to database

## Usage

``` r
addPlatform(conn_handler, platform_tbl, verbose = TRUE)
```

## Arguments

- conn_handler:

  An R object obtained from SigRepo::newConnhandler() (required)

- platform_tbl:

  A Data Frame; Must contain the following column names: platform_name
  (required)

- verbose:

  Logical; whether to print diagnostic messages. Defaults to 'TRUE'.

## Examples
