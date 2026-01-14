# Filter records outside transect area

Filter the records so that no animals fall outside of the searchable
transect area

## Usage

``` r
filter_records_outside_transect_area(records, transects, endcap = "FLAT")
```

## Arguments

- records:

  data.frame of the records with AnimalLongitude and AnimalLatitude
  columns

- transects:

  sf data.frame of the transects

- endcap:

  style of the endcap buffering, see
  [st_buffer](https://r-spatial.github.io/sf/reference/geos_unary.html),
  default is 'FLAT' meaning the searchable area does not extend past the
  end of the transect

## Value

data.frame
