# addUserToCollection

Add a list of users with specific access to a collection in the database

## Usage

``` r
addUserToCollection(
  conn_handler,
  collection_id,
  user_name,
  access_type = c("owner", "editor", "viewer"),
  verbose = TRUE
)
```

## Arguments

- conn_handler:

  An R object obtained from SigRepo::newConnhandler() (required)

- collection_id:

  Database ID of a collection (required)

- user_name:

  A list of users to be added to a collection (required)

- access_type:

  A list of permissions to be given to users in order for them to view
  or manage the collection in the database (required) .

  There are three types of permissions:

  `owner` has Read and Write access to their own uploaded collection.
  `editor` has Read and Write access to collection that other users were
  given them access to. `viewer` has ONLY Read access to collection that
  other users were given them access to.

- verbose:

  Logical; whether to print diagnostic messages. Defaults to 'TRUE'.

## Examples
