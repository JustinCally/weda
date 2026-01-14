# Database Connection

function to easily connect to the weda database

## Usage

``` r
weda_connect(username = "psql_user", password, ...)
```

## Arguments

- username:

  postgres user name (default is psql_user)

- password:

  postgres password (keyring use suggested)

- ...:

  additional arguments passed to `RPostgreSQL::dbConnect()`

## Value

PostgreSQL connection

## Examples

``` r
if (FALSE) { # \dontrun{

weda_connect(password = keyring::key_get(service = "ari-dev-weda-psql-01", username = "psql_user"))

} # }
```
