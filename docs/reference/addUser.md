# addUser

Add a list of users to the database

## Usage

``` r
addUser(conn_handler, user_tbl, verbose = TRUE)
```

## Arguments

- conn_handler:

  An R object obtained from SigRepo::newConnhandler() (required)

- user_tbl:

  A Data Frame; must contain the following column names: user_name,
  user_password, user_email, user_first, user_last, user_affiliation,
  user_role, active (required)

- verbose:

  Logical; whether to print diagnostic messages. Defaults to 'TRUE'

## Examples
