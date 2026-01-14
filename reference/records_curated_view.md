# Curated views

Take raw records and return the most recently modified row for each
record

## Usage

``` r
transect_records_curated_view(con, return_data = FALSE)

transect_curated_view(con, return_data = FALSE)

transect_project_curated_view(con, return_data = FALSE)

records_curated_view(con, return_data = FALSE)

operation_curated_view(con, return_data = FALSE)

project_curated_view(con, return_data = FALSE)
```

## Arguments

- con:

  database connection

- return_data:

  logical flag to return data (TRUE) or sql query (default is FALSE)

## Value

sql or data.frame

## Examples

``` r
if (FALSE) { # \dontrun{
con <- weda_connect(password = keyring::key_get(service = "ari-dev-weda-psql-01",
username = "psql_user"))
DBI::dbExecute(conn = con,
               paste(DBI::SQL("CREATE VIEW camtrap.curated_camtrap_records AS"),
               records_curated_view(con)))
DBI::dbExecute(conn = con,
               paste(DBI::SQL("CREATE VIEW camtrap.curated_camtrap_operation AS"),
               operation_curated_view(con)))
DBI::dbExecute(conn = con,
               paste(DBI::SQL("CREATE VIEW camtrap.curated_project_information AS"),
               project_curated_view(con)))
} # }
```
