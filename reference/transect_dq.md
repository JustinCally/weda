# Transect Data Quality Checks

Assesses the data quality of transect records, transects and project
information. Automatically checks whether columns are present, converts
them to the appropriate class and runs a pointblank check on the data

## Usage

``` r
transect_dq(records, transects, project_information, progress = interactive())
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

- progress:

  show pointblank's step-by-step progress log (default: in interactive
  sessions only)

## Value

list of pointblank objects
