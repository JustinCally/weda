# This function standardises species names in a dataframe

This function standardises species names in a dataframe

## Usage

``` r
standardise_species_names(
  recordTable,
  format = c("scientific", "common"),
  speciesCol = "Species",
  return_data = TRUE
)
```

## Arguments

- recordTable:

  A dataframe containing species names

- format:

  The format of the species names in the dataframe

- speciesCol:

  The column name of the species names in the dataframe

- return_data:

  Whether to return data or just verbose of name conversions

## Value

A dataframe with standardised species names

## Examples

``` r
if (FALSE) { # \dontrun{
standardise_species_names(recordTable = recordTable,
                          format = "scientific",
                          speciesCol = "Species")
                          } # }
```
