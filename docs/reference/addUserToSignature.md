# addUserToSignature

Add a list of users with specific access to a signature in the database

## Usage

``` r
addUserToSignature(
  conn_handler,
  signature_id,
  user_name,
  access_type = c("owner", "editor", "viewer"),
  verbose = TRUE
)
```

## Arguments

- conn_handler:

  An R object obtained from SigRepo::newConnhandler() (required)

- signature_id:

  Database ID of a signature (required)

- user_name:

  A list of users to be added to a signature (required)

- access_type:

  A list of permissions to be given to users in order for them to view
  or manage the signature in the database (required).

  There are three types of permissions:

  `owner` has Read and Write access to their own uploaded signatures.
  `editor` has Read and Write access to signatures that other users were
  given them access to. `viewer` has ONLY Read access to signatures that
  other users were given them access to.

- verbose:

  Logical; whether to print diagnostic messages. Defaults to 'TRUE'.

## Examples
