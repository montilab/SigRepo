# createHashKey

Check api key whether it is valid to access the database

## Usage

``` r
createHashKey(table, hash_var, hash_columns, hash_method = "md5")
```

## Arguments

- table:

  A data frame with specific columns to be used to create the hash key

- hash_var:

  The hash variable to be created

- hash_columns:

  The specific columns to be used to create the hash key

- hash_method:

  The hashing method to create the key. Default `md5`.
