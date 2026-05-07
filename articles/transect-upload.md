# Uploading transect records to database

``` r

library(weda)
library(dplyr)
library(sf)
library(kableExtra)
```

## Brief

We need a process that takes (i) raw proofsafe survey records, (ii) GPS
transect lines in GeoJSON format, and (iii) project information, and
uploads them to the database in a consistent manner that allows transect
survey data across projects to be stored and analysed together.

Transect surveys at ARI follow a **double-observer distance-sampling**
design, where two observers walk the same transect and independently
record detections. Surveys are conducted either during the day
(e.g. koala surveys) or at night via spotlight (e.g. greater glider
surveys). Raw data is captured in the field using
[Proofsafe](https://www.proofsafe.com.au/) forms and exported as CSV
files.

## Input

There are three inputs to this process:

1.  A **proofsafe records table** — the raw CSV export from Proofsafe
    containing animal detections along transects
2.  A **GeoJSON file of GPS transect lines** — spatial line features
    with `SiteID` and `Transect` columns linking back to the records
3.  A **project information table** — a single-row CSV with metadata
    about the project (e.g. project name, target species, survey design)

See the [appendix](#appendix) for the full list of columns expected in
each uploaded table.

## Transect database model

Once this process is completed, data will be uploaded into the `raw`
layer of the `transect` schema on the database. Automatic database views
will then process this raw data into the `curated` and `processed`
layers.

**Raw layer** — all records as uploaded, including any re-uploads
(de-duplication handled by curated views):

- `raw_transect_records` — individual animal detections
- `raw_transects` — transect deployment information (with spatial
  geometry)
- `raw_project_information` — project metadata

**Curated layer** — duplicates removed, most data fields retained:

- `curated_transect_records` — most recent records for each unique
  detection
- `curated_transects` — most recent entry for each transect deployment
- `curated_project_information` — most recent entry for each project

**Processed layer** — derived outputs ready for analysis:

- `processed_transect_presence_absence` — presence/absence of each
  species at each transect × site × iteration combination, with spatial
  geometry attached

## Process

Uploading transect data involves eight steps. The easiest way to
complete these steps is through the interactive Shiny app described
below. Alternatively, the steps can be run directly in R using the
functions described in [Upload using R code](#upload-using-r-code).

**Step 1** — Import the proofsafe records CSV **Step 2** — Import the
GeoJSON transect lines **Step 3** — Import the project information CSV
**Step 4** — Format proofsafe data to database format (applies the
relevant `*_proofsafe_format()` function) **Step 5** — Standardise
species names to VBA taxonomy **Step 6** — Inspect a map of records and
transects **Step 7** — Run data quality checks (all checks must pass
before upload) **Step 8** — Upload to database

## Upload data using the Shiny app

The easiest way to upload transect data is through the hosted Shiny app
at:

**<https://arisci.shinyapps.io/transect-app/>**

*Note: the app requires VPN access and login credentials.*

The app walks you through all eight steps in the sidebar. Download the
example data from the “Download Example Data” button in the app to see
the expected file formats before preparing your own data.

The app currently uses the
[`region_gg_proofsafe_format()`](https://justincally.github.io/weda/reference/koala_proofsafe_format.md)
function in Step 4. If you need to upload **koala** survey data or
standard (non-regional) **greater glider** data, use the R code workflow
below with
[`koala_proofsafe_format()`](https://justincally.github.io/weda/reference/koala_proofsafe_format.md)
or
[`gg_proofsafe_format()`](https://justincally.github.io/weda/reference/koala_proofsafe_format.md)
respectively.

You can also run the app locally from R if needed. Set up a database
connection and launch the app:

*Note: to learn more about database connections, see
[`vignette('database-connect')`](https://justincally.github.io/weda/articles/database-connect.md)*

``` r

# Make sure VPN is running
con <- weda::weda_connect(password = keyring::key_get(service = "ari-dev-weda-psql-01",
                                                      username = "psql_user"))
# Launch the transect app
shiny::runApp(system.file("app/transect-app.R", package = "weda"))
```

## Upload using R code

The following steps replicate what the Shiny app does, but in R code.
This is useful when the app’s default format function does not match
your survey type, or when you want to script a repeatable upload
workflow.

Example data files used below are bundled with the package. The regional
greater glider example uses a raw proofsafe export
(`region_gg_records.csv`) alongside its GPS transect lines and project
metadata:

``` r

proofsafe_path <- system.file("dummydata/transectdata/region_gg_records.csv", package = "weda")
transects_path <- system.file("dummydata/transectdata/region_gg_transects.geojson", package = "weda")
project_path   <- system.file("dummydata/transectdata/region_gg_project.csv", package = "weda")
```

### Step 1 — Load proofsafe records

Load the raw proofsafe CSV export. This is the direct output from
exporting a Proofsafe form — no pre-processing is needed at this stage.

``` r

raw_proofsafe <- readr::read_csv(proofsafe_path, show_col_types = FALSE)

raw_proofsafe %>%
  head(5) %>%
  kbl() %>%
  kable_styling() %>%
  scroll_box(width = "100%")
```

| Business_Id_B | Business_Name_B | Author_Id_F | Author_Name_F | Form_Id_F | Form_Name_F | File_Id_F | File_Name_F | Section_Id_F | Data_Section_Id_F | Parent_Data_Section_Id_F | Section_Id_921_H1 | Data_Section_Id_921_H1 | SiteID_H1 | Transect_H1 | Date_H1 | Start_time_H1 | Observer_H1…18 | ObserverOther_H1 | Observer_H1…20 | GPS_H1 | StatusBurn_H1 | Access_H1 | TransectNotes_H1 | Visibility_H1 | Temp_C_H1 | MoonPhase_H1 | Nightlight_H1 | CloudCover_H1 | Wind_H1 | Precipitation_H1 | FlowerIndex_H1 | Section_Id_922_H2 | Data_Section_Id_922_H2 | P1 Vis_rank_H2 | P2 Vis_rank_H2 | P3 Vis_rank_H2 | P4 Vis_rank_H2 | P5 Vis_rank_H2 | P6 Vis_rank_H2 | End_time_H2 | Notes_H2 | Section_Id_923_I3 | Data_Section_Id_923_I3 | Animal_I3 | AnimObsTime_I3 | SeenHeard_I3 | Species_I3 | Animal_sp_other_I3 | L or R of trans_I3 | Waypoint no.\_I3 | AnimalHeight_I3 | Distance to animal_I3 | Bearing to A.\_I3 | Dist_F_Transect_I3 | Tree species_I3 | Tree_sp_other_I3 | SeenX2_I3 | Comments_I3 |
|---:|:---|---:|:---|---:|:---|---:|:---|---:|---:|---:|---:|---:|:---|---:|:---|:---|:---|:---|---:|:---|:---|:---|:---|:---|---:|---:|:---|---:|:---|:---|:---|---:|---:|:---|:---|:---|:---|:---|:---|:---|:---|---:|---:|:---|:---|:---|:---|:---|:---|:---|---:|---:|---:|---:|:---|:---|:---|:---|
| 1 | Department of Environment | 1 | ARI Contractor | 1 | NA | 1001 | Example Road S1 N1 | 923 | 1001 | 0 | 921 | 1000 | S1 | 1 | 2023-04-15 | 20:00:00 | Other | Jane Smith- Alex Jones | 1 | Other | Control | Access via main road | NA | Poor: thick understorey | 15 | 50 | Medium | 40 | Light breeze (6-11 km/h) | No rain | No trees in flower | 922 | 1000 | 51-75% | 51-75% | 0-25% | 0-25% | 0-25% | 51-75% | 21:30:00 | NA | 923 | 1001 | 01 | 20:45:00 | Heard | Southern Boobook | NA | Left | 01 | NA | 35.00 | 236 | 190 | NA | NA | No | Transect bearing = 335. |
| 1 | Department of Environment | 1 | ARI Contractor | 1 | NA | 1001 | Example Road S1 N1 | 923 | 1002 | 0 | 921 | 1000 | S1 | 1 | 2023-04-15 | 20:00:00 | Other | Jane Smith- Alex Jones | 1 | Other | Control | Access via main road | NA | Poor: thick understorey | 15 | 50 | Medium | 40 | Light breeze (6-11 km/h) | No rain | No trees in flower | 922 | 1000 | 51-75% | 51-75% | 0-25% | 0-25% | 0-25% | 51-75% | 21:30:00 | NA | 923 | 1002 | 03 | 22:20:00 | Seen | Southern Greater Glider | NA | Right | 03 | 28 | 47.11 | 52 | 430 | Peppermint sp. | NA | NA | Transect bearing = 356. |

### Step 2 — Load GPS transect lines

The transect geometry must be provided as a GeoJSON file containing line
features. The GeoJSON must include at least a `SiteID` and `Transect`
column so that spatial lines can be joined to the survey records.

``` r

gps_transects <- sf::st_read(transects_path, quiet = TRUE)

gps_transects %>%
  kbl() %>%
  kable_styling() %>%
  scroll_box(width = "100%")
```

| SiteID | Transect | geometry                     |
|:-------|---------:|:-----------------------------|
| S1     |        1 | LINESTRING (146.8004 -37.90… |

### Step 3 — Create project information

Alongside the survey data, a single-row table is required to record
project-level metadata. You can load this from a CSV (as done in the
app) or construct it directly in R. Check that your `ProjectShortName`
is not already in use by another project with
[`check_unique_project()`](https://justincally.github.io/weda/reference/check_unique_project.md).

``` r

project_information <- readr::read_csv(project_path, show_col_types = FALSE)

project_information %>%
  kbl() %>%
  kable_styling() %>%
  scroll_box(width = "100%")
```

| ProjectName | ProjectShortName | DistanceSampling | TerrestrialArboreal | AllSpeciesTagged | DistanceForAllSpecies | DiurnalNocturnal | ProjectDescription | ProjectLeader |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| Greater Glider Example Survey | gg_example | TRUE | Arboreal | TRUE | FALSE | Nocturnal | Example project for vignette demonstration | First Last |

The required fields are:

| Column | Description |
|----|----|
| `ProjectName` | Full descriptive project name |
| `ProjectShortName` | Short unique identifier used as a key in the database |
| `DistanceSampling` | Logical — whether the survey used distance sampling |
| `TerrestrialArboreal` | `"Terrestrial"` or `"Arboreal"` |
| `AllSpeciesTagged` | Logical — whether all encountered species were recorded |
| `DistanceForAllSpecies` | Logical — whether distances were recorded for all species |
| `DiurnalNocturnal` | `"Diurnal"` or `"Nocturnal"` |
| `ProjectDescription` | Brief description of the project objectives |
| `ProjectLeader` | Name of the project lead |

### Step 4 — Format proofsafe data to database format

This is the key transformation step. The appropriate
`*_proofsafe_format()` function converts the raw proofsafe CSV and GPS
transect lines into the standardised database format, calculating
derived fields such as perpendicular distances and projected animal
locations.

Choose the function that matches your survey type:

| Survey type | Function |
|----|----|
| Koala (diurnal double-observer) | [`koala_proofsafe_format()`](https://justincally.github.io/weda/reference/koala_proofsafe_format.md) |
| Greater glider spotlight (standard) | [`gg_proofsafe_format()`](https://justincally.github.io/weda/reference/koala_proofsafe_format.md) |
| Greater glider spotlight (regional) | [`region_gg_proofsafe_format()`](https://justincally.github.io/weda/reference/koala_proofsafe_format.md) |

For this example we use
[`region_gg_proofsafe_format()`](https://justincally.github.io/weda/reference/koala_proofsafe_format.md):

``` r

formatted_data <- region_gg_proofsafe_format(
  proofsafe          = raw_proofsafe,
  gps_transects      = gps_transects,
  Iteration          = 1L,
  MaxTruncationDistance = 100
)
#> Joining with `by = join_by(Author_Id_F, SiteID_H1, Transect_H1, Date_H1,
#> Observer_H1...20)`
#> Joining with `by = join_by(SiteID, Transect, Species, AnimalID, Date)`

# The output is a list with two elements:
names(formatted_data)
#> [1] "records"   "transects"
```

For koala surveys, the call is equivalent but uses
[`koala_proofsafe_format()`](https://justincally.github.io/weda/reference/koala_proofsafe_format.md):

``` r

formatted_data <- koala_proofsafe_format(
  proofsafe             = raw_proofsafe,
  gps_transects         = gps_transects,
  sp_filter             = "Koala",
  Iteration             = 1L,
  SurveyMethod          = "Diurnal double-observer distance-sampling",
  MaxTruncationDistance = 100
)
```

The returned list contains:

- `$records` — data.frame of formatted animal detections
- `$transects` — sf data.frame of transect deployments with spatial
  geometry

### Step 5 — Standardise species names

Species names in the database follow the Victorian Biodiversity Atlas
(VBA) taxonomy, which requires both a `scientific_name` and a
`common_name` column. Use
[`standardise_species_names()`](https://justincally.github.io/weda/reference/standardise_species_names.md)
to check and append the missing name type from the VBA lookup table
([`weda::vba_name_conversions`](https://justincally.github.io/weda/reference/vba_name_conversions.md)).

First, check whether your species names match the VBA without modifying
the data:

``` r

standardise_species_names(recordTable = formatted_data$records,
                          format      = "scientific",
                          speciesCol  = "Species",
                          return_data = FALSE)
#> Warning in standardise_species_names(recordTable = formatted_data$records, : No match found for Southern Boobook, Southern Greater Glider. Please provide names within the VBA taxa list
#> Warning in max(conversions_grouped$n): no non-missing arguments to max;
#> returning -Inf
#> ->
#> ✖ Southern Boobook
#> ✖ Southern Greater Glider
```

If all names convert successfully, standardise the data:

``` r

formatted_records_std <- standardise_species_names(
  recordTable = formatted_data$records,
  format      = "scientific",
  speciesCol  = "Species",
  return_data = TRUE
)
#> Warning in standardise_species_names(recordTable = formatted_data$records, : No match found for Southern Boobook, Southern Greater Glider. Please provide names within the VBA taxa list
#> Warning in max(conversions_grouped$n): no non-missing arguments to max;
#> returning -Inf
#> ->
#> ✖ Southern Boobook
#> ✖ Southern Greater Glider
```

If a name cannot be matched, you will need to correct it in the source
data to align with VBA conventions. The full VBA lookup can be browsed
with `View(weda::vba_name_conversions)`.

### Step 6 — Inspect a map of records and transects

Before running data quality checks, it is worth visually inspecting the
data to confirm that animal records fall in plausible locations relative
to the transect lines.

``` r

visualise_records(records   = formatted_records_std,
                  transects = formatted_data$transects)
#> Warning in min(cc[, 1], na.rm = TRUE): no non-missing arguments to min;
#> returning Inf
#> Warning in min(cc[, 2], na.rm = TRUE): no non-missing arguments to min;
#> returning Inf
#> Warning in max(cc[, 1], na.rm = TRUE): no non-missing arguments to max;
#> returning -Inf
#> Warning in max(cc[, 2], na.rm = TRUE): no non-missing arguments to max;
#> returning -Inf
#> Warning in min(cc[, 1], na.rm = TRUE): no non-missing arguments to min;
#> returning Inf
#> Warning in min(cc[, 2], na.rm = TRUE): no non-missing arguments to min;
#> returning Inf
#> Warning in max(cc[, 1], na.rm = TRUE): no non-missing arguments to max;
#> returning -Inf
#> Warning in max(cc[, 2], na.rm = TRUE): no non-missing arguments to max;
#> returning -Inf
#> Warning: sf layer has inconsistent datum (+proj=longlat +ellps=GRS80 +no_defs).
#> Need '+proj=longlat +datum=WGS84'
#> Warning: sf layer has inconsistent datum (+proj=longlat +ellps=GRS80 +no_defs).
#> Need '+proj=longlat +datum=WGS84'
#> Warning: sf layer has inconsistent datum (+proj=longlat +ellps=GRS80 +no_defs).
#> Need '+proj=longlat +datum=WGS84'
#> Warning: sf layer has inconsistent datum (+proj=longlat +ellps=GRS80 +no_defs).
#> Need '+proj=longlat +datum=WGS84'
#> Warning: sf layer has inconsistent datum (+proj=longlat +ellps=GRS80 +no_defs).
#> Need '+proj=longlat +datum=WGS84'
```

The map shows:

- **Blue lines/polygons** — transect lines and their truncation-distance
  buffers
- **Red points** — projected animal locations
- **Black lines** — observer-to-animal sight-lines
- **Orange lines** — second-observer projections (where dual-observer
  data exists)

Records that fall outside the transect buffer can be inspected or
removed using
[`filter_records_outside_transect_area()`](https://justincally.github.io/weda/reference/filter_records_outside_transect_area.md).

### Step 7 — Run data quality checks

All data must pass the quality checks before upload. The
[`transect_dq()`](https://justincally.github.io/weda/reference/transect_dq.md)
function runs over 100 checks across the three tables using the
`pointblank` package, covering column presence, data types, value
ranges, coordinate bounds, and distance-sampling constraints.

``` r

dq <- transect_dq(
  records             = formatted_records_std,
  transects           = formatted_data$transects,
  project_information = project_information
)

dq[[1]]  # records
```

[TABLE]

``` r

dq[[2]]  # transects
```

[TABLE]

``` r

dq[[3]]  # project information
```

[TABLE]

Each output table shows:

- **W (Warning)** — yellow dot; data is usable but should be reviewed
- **S (Stop)** — red dot; data cannot be uploaded until the issue is
  resolved
- **N (Notify)** — informational flag for unusual but not invalid values

Upload will only proceed when there are no red dots. Use the **EXT**
download button on any failing row to export the problematic records and
trace them back to your source data.

To confirm all checks have passed programmatically:

``` r

all(sapply(dq, function(x) all(x[["validation_set"]][["all_passed"]])))
```

### Step 8 — Prepare and upload

Once all quality checks pass, prepare the data for upload. This step
generates MD5 hash IDs for each record to prevent duplicates on the
database.

*Note: the bundled example data is intentionally minimal (2 records,
single observer) and will not pass all quality checks. The chunks below
use `eval = FALSE` and are intended to be run with your own complete
survey data.*

``` r

data_for_upload <- prepare_transect_upload(agent_list = dq)
```

Then upload to the database. You will need an active database
connection.

*Note: before uploading, confirm that your `ProjectShortName` is unique
using `check_unique_project(ProjectShortName, con)` if you have not
already done so.*

``` r

con <- weda::weda_connect(password = keyring::key_get(service = "ari-dev-weda-psql-01",
                                                      username = "psql_user"))

upload_transect_data(
  con              = con,
  data_list        = data_for_upload,
  uploadername     = "Firstname Surname",
  schema           = "transect"
)
```

The function appends data to the three raw tables
(`raw_transect_records`, `raw_transects`, `raw_project_information`).
The upload may take a few minutes. Leave your R session running and be
patient.

To upload to the development schema first (recommended when testing a
new project for the first time):

``` r

upload_transect_data(
  con          = con,
  data_list    = data_for_upload,
  uploadername = "Firstname Surname",
  schema       = "transect_dev"
)
```

## Accessing uploaded data

Once uploaded, use the curated view functions to query the most recent
records without duplicates:

``` r

# Curated records (SQL query, collected on demand)
transect_records_curated_view(con, return_data = TRUE)
transect_curated_view(con, return_data = TRUE)
transect_project_curated_view(con, return_data = TRUE)

# Processed presence/absence (all species)
processed_transect_presence_absence(con, return_data = TRUE)

# Presence/absence for a specific species
processed_transect_presence_absence(con,
                                    return_data = TRUE,
                                    species     = "Greater Glider")
```

See
[`vignette('data-download')`](https://justincally.github.io/weda/articles/data-download.md)
for more details on working with database data in R.

## Appendix

A data dictionary is provided in this package (`data(data_dictionary)`)
and also available in the `data_dictionary` schema on the database.
Below is the data dictionary for the `transect` schema:

``` r

data_dictionary %>%
  filter(schema == "transect") %>%
  select(table_name, table_description, column_name, column_class, column_description) %>%
  kbl() %>%
  kable_styling(c("condensed"), full_width = FALSE) %>%
  collapse_rows(1:3, valign = "top") %>%
  scroll_box(width = "100%", height = "1000px")
```

| table_name | table_description | column_name | column_class | column_description |
|:---|:---|:---|:---|:---|
| raw_transect_records | Records of animals detected along transects | SiteID | character | NA |
|  |  | Transect | integer | NA |
|  |  | Iteration | integer | NA |
|  |  | scientific_name | character | NA |
|  |  | common_name | character | NA |
|  |  | SurveyMethod | character | NA |
|  |  | DateTime | POSIXct, POSIXt | NA |
|  |  | AnimalID | character | NA |
|  |  | SeenHeard | character | NA |
|  |  | Adults | integer | NA |
|  |  | Joeys | integer | NA |
|  |  | Individuals | integer | NA |
|  |  | LoR | character | NA |
|  |  | WaypointNo | character | NA |
|  |  | ObserverLatitude | numeric | NA |
|  |  | ObserverLongitude | numeric | NA |
|  |  | AnimalDistance | numeric | NA |
|  |  | AnimalHeight | numeric | NA |
|  |  | AnimalHorizontalDistance | numeric | NA |
|  |  | AnimalAngle | numeric | NA |
|  |  | AnimalBearing | numeric | NA |
|  |  | DistanceFromTransectStart | numeric | NA |
|  |  | AnimalPerpDistance | numeric | NA |
|  |  | TreeSpecies | character | NA |
|  |  | BothSeen | logical | NA |
|  |  | ObservationNotes | character | NA |
|  |  | ObserverPosition | integer | NA |
|  |  | AnimalLongitude | numeric | NA |
|  |  | AnimalLatitude | numeric | NA |
|  |  | ColourForm | character | NA |
|  |  | PhotoID | character | NA |
|  |  | AnimalLongitude2 | numeric | NA |
|  |  | AnimalLatitude2 | numeric | NA |
|  |  | LoR2 | character | NA |
|  |  | SeenOnSameSide | logical | NA |
|  |  | DistanceBetweenAnimalProj | numeric | NA |
| raw_transect_details | Records of when and where transects were undertaken | SiteID | character | NA |
|  |  | Transect | integer | NA |
|  |  | Iteration | integer | NA |
|  |  | ObserverPosition | integer | NA |
|  |  | ObserverName | character | NA |
|  |  | ObserverID | character | NA |
|  |  | Date | Date | NA |
|  |  | StartTime | POSIXct, POSIXt | NA |
|  |  | EndTime | POSIXct, POSIXt | NA |
|  |  | Duration | difftime | NA |
|  |  | Weather | character | NA |
|  |  | Temperature | numeric | NA |
|  |  | TransectNotes | character | NA |
|  |  | MoonPhase | character | NA |
|  |  | Cloud | logical | NA |
|  |  | RelativeHumidity | logical | NA |
|  |  | Wind | character | NA |
|  |  | Precipitation | character | NA |
|  |  | FlowerIndex | character | NA |
|  |  | Access | character | NA |
|  |  | Visibility | character | NA |
|  |  | TransectLength | numeric | NA |
|  |  | MaxTruncationDistance | numeric | NA |
|  |  | TransectType | character | NA |
|  |  | geometry | sfc_MULTILINESTRING, sfc | NA |
| raw_project_information | Details of the project under which transects were searched | ProjectName | character | NA |
|  |  | ProjectShortName | character | NA |
|  |  | DistanceSampling | logical | NA |
|  |  | TerrestrialArboreal | character | NA |
|  |  | AllSpeciesTagged | logical | NA |
|  |  | DistanceForAllSpecies | logical | NA |
|  |  | DiurnalNocturnal | character | NA |
|  |  | ProjectDescription | character | NA |
|  |  | ProjectLeader | character | NA |
