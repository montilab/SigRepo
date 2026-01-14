# updateUser

Update a user in the database

## Usage

``` r
updateUser(
  conn_handler,
  user_name,
  password = NULL,
  email = NULL,
  first_name = NULL,
  last_name = NULL,
  affiliation = NULL,
  role = NULL,
  active = NULL,
  verbose = TRUE
)
```

## Arguments

- conn_handler:

  An R object obtained from SigRepo::newConnhandler() (required)

- user_name:

  Name of a user to be updated (required).

- password:

  Password of a user to be updated. Default is NULL.

- email:

  Email of a user to be updated. Default is NULL.

- first_name:

  First name of a user to be updated. Default is NULL

- last_name:

  Last name of a user to be updated. Default is NULL.

- affiliation:

  Affiliation of the user. Default is NULL.

- role:

  Role of a user to be updated. Choices are admin/editor/viewer.

- active:

  Whether to make a user TRUE (active) or FALSE (inactive). Default is
  NULL.

- verbose:

  Logical; whether or not to print the diagnostic messages. Defaults to
  'TRUE'
