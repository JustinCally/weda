#' Shiny Map UI for Projects
#'
#' @description Shiny module creating an interactive project map
#'
#' @param id module id
#' @param label module label
#' @param custom_css_path custom css path for map
#' @param custom_js_path custom javascript path for module
#' @param colour_vars variables to colour by
#'
#' @return shiny module
#' @export
projectMapUI <- function(id,
                       label = "projectMap",
                       custom_css_path = system.file("app/styles.css", package = "weda"),
                       custom_js_path = system.file("app/gomap.js", package = "weda"),
                       colour_vars) {

  ns <- shiny::NS(id)
  shiny::tabPanel("Project Map",
           shiny::div(class="outer",

               shiny::tags$head(
                 # Include our custom CSS
                 shiny::includeCSS(custom_css_path),
                 shiny::includeScript(custom_js_path)
               ),

               # If not using custom CSS, set height of leafletOutput to a number instead of percent
               leaflet::leafletOutput(outputId = ns("map"), width="100%", height="100%"),

               # Shiny versions prior to 0.11 should use class = "modal" instead.
               shiny::absolutePanel(id = ns("controls"), class = "panel panel-default", fixed = TRUE,
                             draggable = FALSE, top = "auto", left = 20, right = "auto", bottom = 20,
                             width = 330, height = "auto",

                             shiny::h2("Project explorer"),

                             datamods::filter_data_ui(id = ns("project"), show_nrow = TRUE, max_height = NULL),
                             shinyWidgets::pickerInput(ns("colour"), "Marker Colour",
                                                       choices = colour_vars,
                                                       selected = colour_vars[1],
                                                       multiple = FALSE,
                                                       options = shinyWidgets::pickerOptions(
                                                         liveSearch = TRUE,
                                                         liveSearchNormalize = TRUE,
                                                         size = 10
                                                       )),
                             shiny::conditionalPanel("input.colour != 'ProjectName'", ns = ns,
                                              # Only prompt species
                                              shinyWidgets::awesomeCheckbox(
                                                inputId = ns("removeNA"),
                                                label = "Remove NA's",
                                                value = TRUE,
                                                status = "danger")),
                             shinyWidgets::downloadBttn(
                               outputId = ns("downloadData"),
                               style = "bordered",
                               size = "sm",
                               color = "primary"),
                             shinyWidgets::downloadBttn(
                               outputId = ns("downloadVBA"),
                               style = "bordered",
                               label = "Download VBA Data",
                               size = "sm",
                               color = "primary")
               )
           )
  )
}

#' @describeIn projectMapUI
#'
#' @param project_locations data.frame of survey locations
#' @param con database connection
#'
#' @return shiny module
#' @export
projectMapServer <- function(id, project_locations, con) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      # datamods returns TRUE/FALSE picker selections as text and silently drops
      # the filter when the column is logical, so filter on text versions
      filter_locations <- project_locations %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.logical), as.character))

      res_filter <- datamods::filter_data_server(
        "project",
        data = shiny::reactive(filter_locations),
        vars = shiny::reactive(c("ProjectName", "BaitedUnbaited",
                                 "BaitType", "DistanceSampling",
                          "AllSpeciesTagged", "DistanceForAllSpecies",
                          "DateDeploy", "DateRetrieve")),
        name = shiny::reactive("data"),
        defaults = shiny::reactive(NULL),
        drop_ids = FALSE,
        widget_char = "picker",
        widget_num = "slider",
        widget_date = "slider",
        label_na = "NA",
        value_na = TRUE
      )

      output$map <- leaflet::renderLeaflet({
        leaflet::leaflet() %>%
          leaflet::setView(lng = 145, lat = -37, zoom = 6) %>%
          leaflet::addTiles()
      })

      # Detections for the selected species, queried only when the selection
      # changes (not on every filter change); presence is then worked out in
      # memory against the camera locations
      detections <- shiny::reactive({
        shinycssloaders::showPageSpinner(background = "#FFFFFFD0", type = 6, caption = "Querying Database")
        on.exit(shinycssloaders::hidePageSpinner())
        species_detections(con, input$colour)
      }) %>%
        shiny::bindEvent(input$colour)

      shiny::observe({
        colourBy <- input$colour
        if(colourBy %in% weda::vba_name_conversions[["common_name"]]) {
          map_data <- add_species_presence(res_filter$filtered(), detections())

          if(input$removeNA) {
            map_data <- map_data[!is.na(map_data[["Presence"]]),]
          }

        } else {
          map_data <- res_filter$filtered()
        }

        # Filters can return no rows (including briefly while the filter widgets
        # initialise). An empty sf has no point geometry, so addCircleMarkers()
        # would error and end the session; clear the map instead
        if (nrow(map_data) == 0) {
          leaflet::leafletProxy("map") %>%
            leaflet::clearMarkers() %>%
            leaflet::removeControl("legend")
          return()
        }

        if (colourBy == "ProjectName") {
          # the values are categorical
          pal <- leaflet::colorFactor("RdYlBu", map_data[[colourBy]])
          col_col <- colourBy
        } else {
          pal <- leaflet::colorFactor(c("#00B2A9", "#201547"),
                                      map_data[["Presence"]],
                                      na.color = "#e0e0e0")
          col_col <- "Presence"
        }

        labels <- list()
        for(i in seq_len(nrow(map_data))) {

        if(!purrr::is_empty(map_data[["SubStation"]][i]) && !is.na(map_data[["SubStation"]][i]) && map_data[["SubStation"]][i] != "NA") {
          ss <-paste0("<br/><strong>SubStation</strong>:", map_data[["SubStation"]][i])
        } else {
          ss <- ""
        }

        labels[i] <- paste0("<strong>Project</strong>: "
                        , map_data[["ProjectName"]][i]
                        , "<br/>"
                        , "<strong>SiteID</strong>: "
                        , map_data[["SiteID"]][i]
                        , ss
        )
        }

        labels <- lapply(labels, shiny::HTML)

        map_proxy <- leaflet::leafletProxy("map") %>%
          leaflet::clearMarkers() %>%
          leaflet::removeControl("legend") %>%
          leaflet::clearShapes() %>%
          leaflet::setView(lng = 145, lat = -37, zoom = 6) %>%
          leaflet::addCircleMarkers(data = map_data,
                                    fillOpacity=0.6,
                                  fillColor=pal(map_data[[col_col]]),
                                  weight = 2,
                                  color = "black",
                                  label = labels,
                                  labelOptions = leaflet::labelOptions(
                                    style = list("font-weight" = "normal",
                                                 padding = "3px 8px"),
                                    textsize = "10px",
                                    direction = "auto"))

        if (col_col == "ProjectName") {
          # One entry per project gets long, so use a collapsed, scrollable legend
          leaflet::addControl(map_proxy, html = project_legend_html(pal, map_data[["ProjectName"]]),
                              position = "bottomright", layerId = "legend", className = "info legend")
        } else {
          leaflet::addLegend(map_proxy, "bottomright", pal = pal, values = map_data[[col_col]],
                             title = col_col, layerId = "legend")
        }

        # Download data
        output$downloadData <- shiny::downloadHandler(
          filename = function() {
            paste('camtrap_data_', Sys.Date(), '.csv', sep='')
          },
          content = function(dl_con) {
            readr::write_csv(map_data, dl_con)
          }
        )

        output$downloadVBA <- shiny::downloadHandler(
          filename = function() {
            paste('camtrap_vba_data_', Sys.Date(), '.csv', sep='')
          },
          content = function(dl_con) {
            shinycssloaders::showPageSpinner(background = "#FFFFFFD0", type = 6, caption = "Formatting VBA Data")
            vba_data <- vba_format(con = con,
                                   return_data = T,
                                   schema = "camtrap",
                                   ProjectShortName = unique(map_data$ProjectShortName))
            readr::write_csv(vba_data, dl_con)
            shinycssloaders::hidePageSpinner()
          }
        )

      })}

  )
}

#' Collapsible legend of project colours for the project map
#'
#' @param pal leaflet colour palette function
#' @param projects project names (factor or character) shown on the map
#'
#' @noRd
#'
#' @return HTML string
project_legend_html <- function(pal, projects) {
  values <- if (is.factor(projects)) levels(droplevels(projects)) else sort(unique(projects))
  escape <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
  }
  items <- paste0('<div style="white-space: nowrap;"><i style="background:', pal(values),
                  '; opacity: 0.8;"></i>', escape(values), '</div>')
  # Leaflet's control styles hide the native disclosure marker, so add an arrow
  arrow_css <- paste0('<style>.project-legend summary { list-style: none; cursor: pointer; font-weight: bold; }',
                      '.project-legend summary::-webkit-details-marker { display: none; }',
                      '.project-legend summary::before { content: "\\25B8  "; }',
                      '.project-legend[open] summary::before { content: "\\25BE  "; }</style>')
  paste0(arrow_css, '<details class="project-legend"><summary>Projects (', length(values), ')</summary>',
         '<div style="max-height: 40vh; overflow-y: auto; margin-top: 4px; padding-right: 6px;">',
         paste(items, collapse = ""), '</div></details>')
}

#' Cameras that detected any of the selected species
#'
#' @description A light query for the project map: only the distinct
#' camera/species detections for the selected species, rather than building the
#' full presence-absence table on the database
#'
#' @param con database connection
#' @param species common names
#' @param schema schema to query
#'
#' @noRd
#'
#' @return data.frame of ProjectShortName, SiteID, SubStation, Iteration, common_name
species_detections <- function(con, species, schema = "camtrap") {
  dplyr::tbl(con, dbplyr::in_schema(schema, "curated_camtrap_records")) %>%
    dplyr::filter(.data$common_name %in% !!species) %>%
    dplyr::select(dplyr::all_of(c("ProjectShortName", "SiteID", "SubStation", "Iteration", "common_name"))) %>%
    dplyr::distinct() %>%
    dplyr::collect()
}

#' Presence of any selected species at each camera
#'
#' @description Presence is 1 if any selected species was detected at the camera,
#' 0 if not detected but detected elsewhere in the same project, and NA if none
#' of the selected species were recorded in that project (as in
#' processed_SubStation_presence_absence())
#'
#' @param locations camera locations (one row per camera)
#' @param detections output of species_detections()
#'
#' @noRd
#'
#' @return locations with Presence and species_detected columns
add_species_presence <- function(locations, detections) {
  keys <- c("ProjectShortName", "SiteID", "SubStation", "Iteration")
  detections$Iteration <- as.integer(detections$Iteration)
  locations$Iteration <- as.integer(locations$Iteration)

  detected <- detections %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::summarise(species_detected = paste(sort(unique(.data$common_name)), collapse = ", "),
                     .groups = "drop") %>%
    dplyr::mutate(Presence = 1)

  locations %>%
    dplyr::left_join(detected, by = keys) %>%
    dplyr::mutate(Presence = dplyr::case_when(
      !is.na(.data$Presence) ~ 1,
      .data$ProjectShortName %in% detections$ProjectShortName ~ 0,
      TRUE ~ NA_real_))
}
