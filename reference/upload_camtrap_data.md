# Upload camera trap data to database

Upload camera trap data to database

## Usage

``` r
upload_camtrap_data(
  con,
  data_list,
  uploadername,
  tables_to_upload = c("raw_camtrap_records", "raw_camtrap_operation",
    "raw_project_information"),
  schema = "camtrap",
  pa_refresh = TRUE
)
```

## Arguments

- con:

  postgresql connection to ari-dev-weda-psql-01

- data_list:

  list of camera trap records, operations and project information
  (output from prepare_camtrap_upload())

- uploadername:

  name of person uploading data

- tables_to_upload:

  vector (characters) of tables to upload. Default is all of them

- schema:

  schema to upload data to (options are camtrap or camtrap_dev)

- pa_refresh:

  whether to update presence absence tables, default is true

## Examples

``` r
if (FALSE) { # \dontrun{
con <- weda_connect(password = keyring::key_get(service = "ari-dev-weda-psql-01",
username = "psql_user"))
upload_camtrap_data(con = con, data_list = data_list, uploadername = "Justin Cally")
} # }
```
