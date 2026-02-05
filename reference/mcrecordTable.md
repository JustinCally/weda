# Camera Trap Record Table for subset sites and using multiple cores

Camera Trap Record Table for subset sites and using multiple cores

## Usage

``` r
mcrecordTable(
  inDir,
  intermediateDir,
  includeStations,
  overwrite = FALSE,
  ...,
  cores = 1
)
```

## Arguments

- inDir:

  character. Directory containing station directories. It must either
  contain images in species subdirectories (e.g.
  inDir/StationA/SpeciesA) or images with species metadata tags (without
  species directories, e.g. inDir/StationA).

- intermediateDir:

  character. Directory path to store intermediate rds objects, which are
  one rds (intermediate file) per station. This directory should not
  have stations, data or files that are not intermediate files.

- includeStations:

  character. (OPTIONAL) Subset of stations to extract data for within a
  directory. Can be useful if only wanting to extract data for a subset
  of sites within a directory. If no argument is given function will
  default to all stations. Otherwise, provide a character vector of
  station names (matching the subdirectory names in inDir).

- overwrite:

  Re-extract data for sites where an intermediate file already exists.
  The default (FALSE), ensures data is not re-extracted for sites that
  already have intermediate files. Allowing you to pick up where you
  left off for extractions that take a long time.

- ...:

  Additional arguments passed to
  [`recordTable`](https://jniedballa.github.io/camtrapR/reference/recordTable.html)

- cores:

  Number of cores to use

## Value

data.frame (same as
[`recordTable`](https://jniedballa.github.io/camtrapR/reference/recordTable.html))

## Examples

``` r
int.dir <- tempdir()
raw_camtrap_records <- mcrecordTable(inDir  = system.file("dummydata/images", package = "weda"),
                                    IDfrom = "metadata",
                                    cameraID = "directory",
                                    stationCol = "SiteID",
                                    camerasIndependent = TRUE,
                                    timeZone = Sys.timezone(location = TRUE),
                                    metadataSpeciesTag = "Species",
                                    removeDuplicateRecords = FALSE,
                                    returnFileNamesMissingTags = TRUE,
                                    includeStations = c("832", "2602"),
                                    intermediateDir = int.dir,
                                    overwrite = FALSE, cores = 1)
#> Warning: cannot open file '/tmp/RtmpQxuCye/bslib-51abec2faeec528d2585accfc5baff61': it is a directory
#> Error in gzfile(file, "rb"): cannot open the connection
```
