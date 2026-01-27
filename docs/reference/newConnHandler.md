# newConnHandler

Create a handler to connect to a remote database.

## Usage

``` r
newConnHandler(
  dbname = "sigrepo",
  host = "sigrepo.org",
  port = 3306,
  user = "guest",
  password = "guest",
  api_host = "sigrepo.org",
  api_port = 8020
)
```

## Arguments

- dbname:

  Name of MySQL database (required)

- host:

  Name of the server where MySQL database is hosted on (required)

- port:

  Port on the server to connect to MySQL database (required)

- user:

  Name of user to establish the connection (required)

- password:

  Password associated with the user (required)

- api_host:

  Name of the server where the API is hosted on (required)

- api_port:

  Port on the server to access the API (required)

## Value

A list object that contains all the items that are passed in the
arguments used for database connection.

## Examples
