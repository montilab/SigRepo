# searchOrganism

Search for an organism in the database, can handle a singular organism
or list

## Usage

``` r
searchOrganism(conn_handler, organism = NULL, verbose = TRUE)
```

## Arguments

- conn_handler:

  An R object obtained from SigRepo::newConnhandler() (required)

- organism:

  an organism, or list of organisms to search by. Default is NULL which
  will return all of the organisms in the database.

- verbose:

  Logical; whether or not to print the diagnostic messages. Defaults to
  'TRUE'.

## Examples
