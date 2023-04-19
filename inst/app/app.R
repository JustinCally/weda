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
project_data <- dplyr::tbl(con,
                           dbplyr::in_schema("camtrap", "curated_project_information")) %>%
  dplyr::collect() %>%
  mutate(DistanceSampling = case_when(DistanceSampling == "1" ~ TRUE,
                                      TRUE ~ FALSE),
         AllSpeciesTagged = case_when(AllSpeciesTagged == "1" ~ TRUE,
                                      TRUE ~ FALSE))

species_presence <- dplyr::tbl(con,
                               dbplyr::in_schema("camtrap", "processed_site_substation_presence_absence")) %>%
  dplyr::collect() %>%
  reshape2::dcast(formula = ProjectShortName + SiteID + SubStation + Iteration ~ common_name, value.var = "Presence")

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
  left_join(species_presence, by = join_by(ProjectShortName, SiteID, SubStation, Iteration)) %>%
  arrange(desc(DateDeploy))

col.vars <- c("ProjectName", colnames(species_presence)[-c(1:3)])

# Define UI for data upload app ----
ui <- navbarPage("weda", id="nav",

                 tabPanel("Map",
                          div(class="outer",

                              tags$head(
                                # Include our custom CSS
                                includeCSS("styles.css"),
                                includeScript("gomap.js")
                              ),

                              # If not using custom CSS, set height of leafletOutput to a number instead of percent
                              leafletOutput("map", width="100%", height="100%"),

                              # Shiny versions prior to 0.11 should use class = "modal" instead.
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = "auto", left = 60, right = "auto", bottom = 60,
                                            width = 330, height = "auto",

                                            h2("Project explorer"),

                                            filter_data_ui("project", show_nrow = TRUE, max_height = NULL),
                                            shinyWidgets::pickerInput("colour", "Colour",
                                                                      choices = col.vars,
                                                                      selected = "ProjectName",
                                                                      multiple = FALSE,
                                                                      options = shinyWidgets::pickerOptions(
                                                                        liveSearch = TRUE,
                                                                        liveSearchNormalize = TRUE,
                                                                        size = 10
                                                                      )),
                                            conditionalPanel("input.colour != 'ProjectName'",
                                                             # Only prompt species
                                                             shinyWidgets::awesomeCheckbox(
                                                               inputId = "removeNA",
                                                               label = "Remove NA's",
                                                               value = TRUE,
                                                               status = "danger"))
                          )
                 )
))

# Define server logic to read selected file ----
server <- function(input, output) {

  res_filter <- filter_data_server(
    "project",
    data = reactive(cam_locations),
    vars = reactive(c("ProjectName", "DistanceSampling",
                      "AllSpeciesTagged", "BaitedUnbaited",
                      "BaitType", "ProjectStart",
                      "ProjectEnd")),
    name = reactive("data"),
    defaults = reactive(NULL),
    drop_ids = FALSE,
    widget_char = "picker",
    widget_num = "slider",
    widget_date = "slider",
    label_na = "NA",
    value_na = TRUE
  )



  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = 145, lat = -37, zoom = 6) %>%
      addTiles()
  })

  observe({
    colourBy <- input$colour
if(input$removeNA) {
  map_data <- res_filter$filtered()[!is.na(res_filter$filtered()[[colourBy]]),]
} else {
    map_data <- res_filter$filtered()
}

    if (colourBy == "ProjectName") {
      # the values are categorical
      pal <- colorFactor("PuOr", map_data[[colourBy]])
    } else {
      pal <- colorFactor("RdYlBu", map_data[[colourBy]], na.color = "#e0e0e0")
    }



  leafletProxy("map") %>%
      clearMarkers() %>%
      removeControl("legend") %>%
      clearShapes() %>%
      setView(lng = 145, lat = -37, zoom = 6) %>%
      addCircleMarkers(data = map_data, fillOpacity=0.8,
                       fillColor=pal(map_data[[input$colour]]), weight = 2, color = "black") %>%
      addLegend("bottomright", pal=pal, values=map_data[[input$colour]], title=colourBy, layerId = "legend")

  })

}
# Run the app ----
shinyApp(ui, server)
