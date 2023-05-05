#' Run the camera trap app
#'
#' @description An app that allows users to view camera trap coverage survey coverage,
#' download data and upload data
#'
#' @param con database connection
#'
#' @return shiny app
#' @export
#'
#' @examples
#' \dontrun{
#' con <- weda_connect(password = keyring::key_get(service = "ari-dev-weda-psql-01",
#' username = "psql_user"))
#' camtrap_app(con = con)
#' }
camtrap_app <- function(con) {
  # Setup load packages
  options(shiny.maxRequestSize = 30*1024^2)
  shiny::addResourcePath("sbs", system.file("www", package="shinyBS"))
  # Database Connection: supply to app
  # con <- weda::weda_connect(password = keyring::key_get(service = "ari-dev-weda-psql-01", username = "psql_user"))

  # Load data
  ## Project data
  project_data <- dplyr::tbl(con,
                             dbplyr::in_schema("camtrap", "curated_project_information")) %>%
    dplyr::collect()
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
                     by = dplyr::join_by(ProjectShortName)) %>%
    dplyr::select(ProjectName, dplyr::everything()) %>%
    dplyr::group_by(ProjectShortName) %>%
    dplyr::mutate(ProjectStart = min(DateDeploy),
                  ProjectEnd = max(DateRetrieve)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(DateDeploy) %>%
    dplyr::mutate(ProjectName = forcats::fct_reorder(ProjectName, DateDeploy, .desc = T))

  col.vars <- c("ProjectName", species_names)

  # Define UI for data upload app ----
  ui <- shiny::navbarPage("weda", id="nav",
                          weda::projectMapUI(id = "map", colour_vars = col.vars),
                          weda::dataUploadpUI(id = "upload")
  )

  # Define server logic to read selected file ----
  server <- function(input, output) {
    weda::projectMapServer(id = "map",
                           project_locations = cam_locations,
                           con = con)

    weda::dataUploadServer(id = "upload", con = con)
  }
  # Run the app ----
  shiny::shinyApp(ui, server)
}
