# searchUser

Get phenotypes in the database

## Usage

``` r
searchUser(conn_handler, user_name = NULL, verbose = TRUE)
```

## Arguments

- conn_handler:

  An R object obtained from SigRepo::newConnhandler() (required)

- user_name:

  A list of user names to search by. Defaults to 'NULL', which will
  return all of the users in the database

- verbose:

  Logical; whether or not to print the diagnostic messages. Defaults to
  'TRUE'.

## Examples
