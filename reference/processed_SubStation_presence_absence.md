# Presence/Absence Views

presence-absence data views for each site/substation (including a daily
presence-absence)

## Usage

``` r
processed_SubStation_presence_absence(
  con,
  return_data = FALSE,
  daily = FALSE,
  species = "all"
)

processed_transect_presence_absence(con, return_data = FALSE, species = "all")
```

## Arguments

- con:

  database connection

- return_data:

  logical flag to return data (TRUE) or sql query (default is FALSE)

- daily:

  logical flag to make the query/data be at a daily level (TRUE), or per
  substation (FALSE)

- species:

  whether to make table for 'all' species (default), otherwise accepts a
  vector of common names (see vba_name_conversions)

## Value

sql or data.frame

## Examples

``` r
if (FALSE) { # \dontrun{
con <- weda_connect(password = keyring::key_get(service = "ari-dev-weda-psql-01",
username = "psql_user"))
DBI::dbExecute(conn = con,
               paste(SQL("CREATE VIEW camtrap.processed_site_substation_presence_absence AS"),
               processed_SubStation_presence_absence(con = con, return_data = FALSE)))
} # }
```
