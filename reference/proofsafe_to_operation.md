# Proofsafe to operation

Take the csv results file from proofsafe and format it into the
operations table format.

## Usage

``` r
proofsafe_to_operation(
  data,
  sensitivity,
  delay,
  photos_per_trigger,
  quiet_period
)
```

## Arguments

- data:

  data.frame/tbl from reading in the proofsafe results csv

- sensitivity:

  Sensitivity of the camera (e.g. "High", or "Very High")

- delay:

  Settings of the camera delay (e.g. Rapidfire)

- photos_per_trigger:

  Settings of how many photos per

- quiet_period:

  How long the camera is delayed after each trigger (e.g. usually 0)

## Value

data.frame

## Examples
