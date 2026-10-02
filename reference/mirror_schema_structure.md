# Mirror a schema's structure (without data) into another schema

Recreates the views, materialized views, indexes and functions of one
schema (e.g. `camtrap`) in another (e.g. `camtrap_dev`) so the
development schema behaves like production without copying production
data. Definitions are read from the PostgreSQL catalog, so they match
what is actually on the server.

Base tables missing from the target are created empty
(`LIKE ... INCLUDING ALL`), except tables in `copy_data_tables`
(reference data such as the VBA name list), which are also copied.
Objects that already exist in the target are skipped. Materialized views
are built from the target schema's own tables.

By default nothing is changed on the database: the SQL is written to
`sql_file` for review. Set `apply = TRUE` to run it in a single
transaction.

## Usage

``` r
mirror_schema_structure(
  con,
  from = "camtrap",
  to = "camtrap_dev",
  sql_file = paste0(to, "_setup.sql"),
  apply = FALSE,
  copy_data_tables = "vba_name_conversions"
)
```

## Arguments

- con:

  database connection

- from:

  schema to copy the structure from

- to:

  schema to create the structure in

- sql_file:

  file to write the generated SQL to (NULL to skip)

- apply:

  logical, run the SQL on the database (default FALSE)

- copy_data_tables:

  base tables whose data should also be copied when created

## Value

generated SQL statements (invisibly)

## Examples

``` r
if (FALSE) { # \dontrun{
con <- weda_connect(password = keyring::key_get(service = "ari-dev-weda-psql-01",
username = "psql_user"))
# Review the SQL first
mirror_schema_structure(con, from = "camtrap", to = "camtrap_dev")
# Then apply it
mirror_schema_structure(con, from = "camtrap", to = "camtrap_dev", apply = TRUE)
} # }
```
