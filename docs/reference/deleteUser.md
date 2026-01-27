# deleteUser

Delete a user from the database

## Usage

``` r
deleteUser(conn_handler, user_name, verbose = TRUE)
```

## Arguments

- conn_handler:

  An R object obtained from SigRepo::newConnhandler() (required)

- user_name:

  Name of the user to be deleted (required).

- verbose:

  Logical; whether or not to print the diagnostic messages. Default is
  `TRUE`.

## Examples
