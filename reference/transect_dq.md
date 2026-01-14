# Transect Data Quality Checks

Assesses the data quality of transect records, transects and project
information. Automatically checks whether columns are present, converts
them to the appropriate class and runs a pointblank check on the data

## Usage

``` r
transect_dq(records, transects, project_information)
```

## Arguments

- records:

  this is the dataframe that contains the records of animals on
  transects

- transects:

  this is the dataframe that contains the information about the transect
  location and time it was surveyed

- project_information:

  this is the dataframe that contains the information about the project

## Value

list of pointblank objects
