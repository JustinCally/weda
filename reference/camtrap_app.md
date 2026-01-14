# Run the camera trap app

An app that allows users to view camera trap coverage survey coverage,
download data and upload data

## Usage

``` r
camtrap_app(con)

transect_app(con)
```

## Arguments

- con:

  database connection

## Value

shiny app

## Examples

``` r
if (FALSE) { # \dontrun{
con <- weda_connect(password = keyring::key_get(service = "ari-dev-weda-psql-01",
username = "psql_user"))
camtrap_app(con = con)
} # }
```
