# Setup load packages
library(shiny)
library(dplyr)
library(leaflet)
library(datamods)
# Database Connection
con <- RPostgreSQL::dbConnect(odbc::odbc(),
                              Driver = "PostgreSQL Driver",
                              Server = '10.110.7.201',
                              Database = 'ari-dev-weda-psql-01',
                              UID = "psql_user",
                              PWD = keyring::key_get(service = "ari-dev-weda-psql-01", username = "psql_user"),
                              Port = 5432,
                              sslmode = 'require',
                              maxvarcharsize = 0) # issue with binary representation of sf

# Load data
## Project data
project_data <- dplyr::tbl(con,
                           dbplyr::in_schema("camtrap", "curated_project_information")) %>%
  dplyr::collect() %>%
  mutate(DistanceSampling = case_when(DistanceSampling == "1" ~ TRUE,
                                      TRUE ~ FALSE),
         AllSpeciesTagged = case_when(AllSpeciesTagged == "1" ~ TRUE,
                                      TRUE ~ FALSE))
## Lazy SPecies Presence
species_presence <- dplyr::tbl(con,
                               dbplyr::in_schema("camtrap", "processed_site_substation_presence_absence"))

species_names <- species_presence %>%
  dplyr::select(common_name) %>%
  dplyr::distinct() %>%
  dplyr::collect() %>%
  dplyr::pull()

## Camera Locations
cam_locations <- dplyr::tbl(con,
                            dbplyr::in_schema("camtrap", "curated_camtrap_operation")) %>%
  dplyr::collect() %>%
  sf::st_as_sf(., coords = c("Longitude", "Latitude"), crs = 4283) %>%
  dplyr::left_join(project_data %>%
                     dplyr::select(-Timestamp, -Uploader),
                   by = join_by(ProjectShortName)) %>%
  dplyr::select(ProjectName, dplyr::everything()) %>%
  dplyr::group_by(ProjectShortName) %>%
  dplyr::mutate(ProjectStart = min(DateDeploy),
                ProjectEnd = max(DateRetrieve)) %>%
  dplyr::ungroup() %>%
  arrange(desc(DateDeploy))

col.vars <- c("ProjectName", species_names)

# Define UI for data upload app ----
ui <- navbarPage("weda", id="nav",
                 projectMapUI(id = "map", colour_vars = col.vars)
                 )

# Define server logic to read selected file ----
server <- function(input, output) {
  projectMapServer(id = "map", project_locations = cam_locations, presence_absence_ldf = species_presence)
}
# Run the app ----
shinyApp(ui, server)
