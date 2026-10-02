# Camera Trap Data Quality Checks

Assesses the data quality of camera trap records, operations and project
information. Automatically checks whether columns are present, converts
them to the appropriate class and runs a pointblank check on the data

## Usage

``` r
camera_trap_dq(
  camtrap_records,
  camtrap_operation,
  project_information,
  con = NULL,
  schema = "camtrap",
  progress = interactive()
)
```

## Arguments

- camtrap_records:

  this is the dataframe that contains the camera trap records
  (recordTable from camtrapR)

- camtrap_operation:

  this is the dataframe that contains the information about the camera
  trap operation

- project_information:

  this is the dataframe that contains the information about the project

- con:

  optional database connection. If supplied, the project short name and
  full name are checked against projects already on the database

- schema:

  schema to check existing projects in (camtrap or camtrap_dev)

- progress:

  show pointblank's step-by-step progress log (default: in interactive
  sessions only)

## Value

list of pointblank objects
