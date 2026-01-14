# VBA Upload format for camera trap data

A presence-absence view of camera trap surveys using the VBA upload
template

## Usage

``` r
vba_format(
  con,
  ProjectShortName = "all",
  return_data = FALSE,
  schema = "camtrap"
)
```

## Arguments

- con:

  database connection

- ProjectShortName:

  ProjectShortName field

- return_data:

  logical flag to return data (TRUE) or sql query (default is FALSE)

- schema:

  which schema to use

## Value

sql or data.frame

## Examples

``` r
 if (FALSE) { # \dontrun{
con <- weda_connect(password = keyring::key_get(service = "ari-dev-weda-psql-01",
username = "psql_user"))
DBI::dbExecute(conn = con,
               paste(SQL("CREATE VIEW camtrap.processed_vba_format AS"),
               vba_format(con = con, return_data = FALSE)))
} # }
```
