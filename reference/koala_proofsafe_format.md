# Format proofsafe data for database

These functions format the datasets from proofsafe alongside gps data of
the transect lines for koala or greater glider (gg) surveys

## Usage

``` r
koala_proofsafe_format(
  proofsafe,
  gps_transects,
  sp_filter = "Koala",
  Iteration,
  SurveyMethod = "Diurnal double-observer distance-sampling",
  MaxTruncationDistance
)

gg_proofsafe_format(
  proofsafe,
  gps_transects,
  Iteration,
  SurveyMethod = "Spotlight double-observer distance-sampling",
  MaxTruncationDistance
)

region_gg_proofsafe_format(
  proofsafe,
  gps_transects,
  Iteration,
  SurveyMethod = "Spotlight double-observer distance-sampling",
  MaxTruncationDistance = 100
)
```

## Arguments

- proofsafe:

  data.frame of data directly from proofsafe

- gps_transects:

  line transects, formatted as an sf object with columns for SiteID and
  Transect

- sp_filter:

  species filter

- Iteration:

  integer of the season or repeat visit number of the survey (e.g. 1 for
  the first time a sites been surveyed or 2 if it has been surveyed once
  before)

- SurveyMethod:

  character type of method used to detect animals

- MaxTruncationDistance:

  The maximum distance in metres that observations were made out to
  (e.g. 100)

## Value

list of data.frame for records and sf for transects
