# Camera Trap Data Quality Checks

Assesses the data quality of camera trap records, operations and project
information. Automatically checks whether columns are present, converts
them to the appropriate class and runs a pointblank check on the data

## Usage

``` r
camera_trap_dq(camtrap_records, camtrap_operation, project_information)
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

## Value

list of pointblank objects
