# Data Dictionary

A data dictionary of the database

## Usage

``` r
data(data_dictionary)
```

## Format

A data frame with 131 rows and 7 variables

## Details

- schema. Schema name

- table_name. Name of the table

- table_description. Description of the purpose of the table

- table_type. Level of table processing (raw, curated or processed)

- column_name. Name of the column/field

- column_class. Class of the column as per R conventions

- column_description. Description of the column purpose

- darwin_standard_core. Darwin standard core matching field

- derived_from. Where the data is derived from (e.g. user or camtrapR)
