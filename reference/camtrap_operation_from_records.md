# Operation from records

Operation from records

## Usage

``` r
camtrap_operation_from_records(records, get_cam_model = TRUE)
```

## Arguments

- records:

  camera trap records from camtrapR with Species, SiteID and SubStation

- get_cam_model:

  logical flag (default is TRUE) of whether you want to get cam model
  and serial number (drive with photos needs to be attached)

## Value

data.frame

## Examples

``` r
library(weda)
raw_camtrap_records <- camtrapR::recordTable(inDir  = system.file("dummydata/images",
package = "weda"),
                                             IDfrom = "metadata",
                                             cameraID = "directory",
                                             stationCol = "SiteID",
                                             camerasIndependent = TRUE,
                                             timeZone = Sys.timezone(location = TRUE),
                                             metadataSpeciesTag = "Species",
                                             removeDuplicateRecords = FALSE,
                                             returnFileNamesMissingTags = TRUE) %>%
dplyr::rename(SubStation = Camera) %>%
dplyr::mutate(SubStation = dplyr::case_when(SiteID == SubStation ~ NA_character_,
                                            TRUE ~ SubStation),
              Iteration = 1L)

 op_table <- camtrap_operation_from_records(raw_camtrap_records)
#> Metadata of:
#> /home/runner/work/_temp/Library/weda/dummydata/images/2602/2602__2021-10-17__01-32-47(5).JPG
#> Metadata of:
#> /home/runner/work/_temp/Library/weda/dummydata/images/43134/43134__2021-11-16__18-15-26(22).JPG
#> Metadata of:
#> /home/runner/work/_temp/Library/weda/dummydata/images/56505/A/56505__2021-11-30__08-50-17(2).JPG
#> Metadata of:
#> /home/runner/work/_temp/Library/weda/dummydata/images/56505/B/56505__2021-11-30__08-50-57(59).JPG
#> Metadata of:
#> /home/runner/work/_temp/Library/weda/dummydata/images/832/832__2021-10-21__14-30-38(8).JPG
#> Metadata of:
#> /home/runner/work/_temp/Library/weda/dummydata/images/9941/9941__2021-10-14__18-09-19(13).JPG
```
