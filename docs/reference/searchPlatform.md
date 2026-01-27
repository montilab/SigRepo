# searchPlatform

Search for platform in the database

## Usage

``` r
searchPlatform(conn_handler, platform_name = NULL, verbose = TRUE)
```

## Arguments

- conn_handler:

  An R object obtained from SigRepo::newConnhandler() (required)

- platform_name:

  A platform, or a list of platform names to be looked up. Defaults to
  'NULL', which will return all of the platforms in the database.

- verbose:

  Logical; whether or not to print the diagnostic messages. Defaults to
  'TRUE'.

## Examples
