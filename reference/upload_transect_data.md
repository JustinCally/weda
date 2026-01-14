# Upload camera trap data to database

Upload camera trap data to database

## Usage

``` r
upload_transect_data(
  con,
  data_list,
  uploadername,
  tables_to_upload = c("raw_transect_records", "raw_transects",
    "raw_project_information"),
  schema = "transect"
)
```

## Arguments

- con:

  postgresql connection to ari-dev-weda-psql-01

- data_list:

  list of transect records, transects and project information (output
  from prepare_transect_upload())

- uploadername:

  name of person uploading data

- tables_to_upload:

  vector (characters) of tables to upload. Default is all of them

- schema:

  schema to upload data to (options are transect or transect_dev)

## Examples

``` r
if (FALSE) { # \dontrun{
con <- weda_connect(password = keyring::key_get(service = "ari-dev-weda-psql-01",
username = "psql_user"))
upload_transect_data(con = con, data_list = data_list, uploadername = "Justin Cally")
} # }
```
