# Visualise transects, records and sight-lines

View an interactive map of the transects and records as well as the
lines from where the observer sighted them and the distance between
projected animal locations

## Usage

``` r
visualise_records(records, transects, endcap = "FLAT")
```

## Arguments

- records:

  data.frame of the records with AnimalLongitude and AnimalLatitude
  columns

- transects:

  sf of the transects searched

- endcap:

  style of the endcap buffering, see
  [st_buffer](https://r-spatial.github.io/sf/reference/geos_unary.html),
  default is 'FLAT' meaning the searchable area does not extend past the
  end of the transect

## Value

mapview widget
