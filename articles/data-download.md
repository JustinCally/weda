# Using and downloading data

``` r
library(weda)
library(dplyr)
library(dbplyr)
library(camtrapR)
```

## Brief

Interacting with data on the ARI database can range from a
straightforward viewing and downloading through the shiny app, or more
complex and custom SQL queries. In this vignette we go through several
steps you can take to download and interact with data.

## Interacting with data on the app

The easiest way to interact with the database, and download/view
presence-absence data and VBA formatted data is by launching the app.

``` r
# Make sure VPN is running
con <- weda::weda_connect(password = keyring::key_get(service = "ari-dev-weda-psql-01",
                                                      username = "psql_user"))
weda::camtrap_app(con = con)
```

On the map pane you can download a presence-absence view of data for a
species, details about camera locations for a project/multiple projects.
You can also download a VBA formatted view of the data.

## Using `dbplyr` to collect data in R

`dbplyr` is a database-specific implementation of the commonly used and
intuitive `dplyr` package. This means that if you are familiar with
`dplyr`/`tidyverse` functions you should find it relatively
straightforward to learn how to pull down, `select`, `filter`, `join`
and `mutate` data. The main data we can pull down from the `camtrap`
schema are:

- `curated_camtrap_operation`
- `curated_camtrap_records`  
- `curated_project_information`  
- `processed_site_substation_presence_absence`  
- `processed_site_substation_daily_presence_absence`

### Lazy views of the data

`dbplyr` allows for lazy evaluation of queries, meaning that data will
not be fully returned until a
[`collect()`](https://dplyr.tidyverse.org/reference/compute.html) is
called at the end of the code/query:

``` r
# This doesn't return any data, just a dbplyr query to be evaluated
query <- tbl(con, in_schema("camtrap", "curated_project_information"))

# return the data with `collect()`
project_data <- query %>%
  collect()
```

In-between the initial query and the collection you can chuck in all
your dplyr filters, selects and mutates:

``` r
project_data_filtered <- query %>%
  filter(DistanceSampling == TRUE & AllSpeciesTagged == TRUE) %>%
  select(ProjectName, ProjectShortName, DistanceSampling, AllSpeciesTagged) %>% 
  mutate(StudyType = "Distance Sampling") %>%
  collect()
```

### Obtaining camera trap records and operations for given projects

Let’s say we want to do an analysis on Emus. We need to pull down the
emu records and the operation data for the cameras

``` r
# filter for emus and projects that had distance sampling and all species tagged
emu_records <- tbl(con, in_schema("camtrap", "curated_camtrap_records")) %>%
  filter(common_name == "Emu" & ProjectShortName %in% !!project_data_filtered$ProjectShortName) %>%
  collect()

# operation tables 
emu_operations_query <- tbl(con, in_schema("camtrap", "curated_camtrap_operation")) %>%
  filter(ProjectShortName %in% !!project_data_filtered$ProjectShortName) 

emu_operations <- emu_operations_query %>% collect()
```

With this raw data we can format it for distance sampling analysis or
other types of analyses. However, if you are just after data for
occupancy analysis there are some existing tools to help you.

## Presence-absence data

### Get VBA data

While the VBA format we will eventually use is still under consideration
this function (`vba_format`) will give you the data from a project in
the template of the VBA, this means you just need to bulk upload it to
VBA.

``` r
VBA_statewide <- vba_format(con, ProjectShortName = "StatewideDeer", return_data = TRUE)
```

### Presence-absence tables

Two presence-absence tables exist on the database. A presence-absence
for a given camera for the whole deployment and a daily
presence-absence.

*Note: We define absence as the absence of any tagged images of a
species at a site, IF THAT SPECIES WAS TAGGED AT ANOTHER CAMERA IN THAT
PROJECT. This means that if the project did not record any Black
Panthers you will not see an absence or ‘0’ for black panthers in the
data.*

``` r
emu_pa <- tbl(con, in_schema("camtrap", "processed_site_substation_presence_absence")) %>%
  filter(common_name == "Emu" & ProjectShortName == "StatewideDeer")

# use collect if you want to collect the above

# you can also join this presence absence data to the operations data (which contains lat/longs)
# joining can be done lazily (server side): But have to remove SubStation Column before joining as it is all NAs
emu_pa_op <- emu_pa %>%
  select(-SubStation) %>%
  left_join(emu_operations_query) %>%
  collect()
```

### Getting detection histories for occupancy analysis

weda also has a function to pull down data for a project and species for
occupancy analysis using `unmarked` or `ubms`. This allows you to get
the detection histories of a species in a project with one function
([`DBdetectionHistory()`](https://justincally.github.io/weda/reference/DBdetectionHistory.md)).

``` r
# Get emu data for each site
EmuDH <- DBdetectionHistory(con,
                            ProjectShortName = "StatewideDeer", #proj name
                            Species = "Emu", #species
                            Iteration = 1, #iteration
                            byCamera = FALSE, #no substrations
                            occasionStartTime = 0, #start at midnight
                            occasionLength = 1, #daily PA
                            includeEffort = TRUE, 
                            scaleEffort = FALSE) #fractional deployment
```

This then allows for an easy integration with unmarked or ubms for
occupancy analysis:

``` r
# load the unmarked package
library(unmarked)

# make the unmarked frame
# Note the site covariates are nothing of interest here it is just 
# the presence_absence table with the lat/longs
# Using the lat/longs you could extract more informative predictors from rasters or shapefiles
umf <- unmarkedFrameOccu(y = EmuDH[[1]], 
              siteCovs = emu_pa_op)

emu_occu <- occu(~ 1 ~ 1, data = umf)

summary(emu_occu)
```
