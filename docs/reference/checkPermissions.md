# checkPermissions

Check api key whether it is valid to access the database

## Usage

``` r
checkPermissions(
  conn,
  action_type = c("SELECT", "INSERT", "UPDATE", "DELETE", "CREATE USER"),
  required_role = c("admin", "editor", "viewer")
)
```

## Arguments

- conn:

  An established connection to database using SigRepo::newConnhandler()

- action_type:

  A list of actions that the connected user can perform. Options:
  SELECT, INSERT, UPDATE, DELETE, CREATE USER

- required_role:

  The required role of the user who can perform the action.
