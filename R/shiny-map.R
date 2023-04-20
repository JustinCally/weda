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
                       custom_css_path = "styles.css",
                       custom_js_path = "gomap.js",
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
                             draggable = TRUE, top = "auto", left = 60, right = "auto", bottom = 60,
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
                                                status = "danger"))
               )
           )
  )
}

#' @describeIn projectMapUI
#'
#' @param project_locations data.frame of survey locations
#' @param presence_absence_ldf presence-absence lazy data.frame
#'
#' @return
#' @export
projectMapServer <- function(id, project_locations, presence_absence_ldf) {

  moduleServer(
    id,
    function(input, output, session) {

      res_filter <- datamods::filter_data_server(
        "project",
        data = shiny::reactive(project_locations),
        vars = shiny::reactive(c("ProjectName", "DistanceSampling",
                          "AllSpeciesTagged", "BaitedUnbaited",
                          "BaitType", "ProjectStart",
                          "ProjectEnd")),
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

      shiny::observe({
        shinycssloaders::showPageSpinner(background = "#FFFFFFD0", type = 6, caption = "Querying Database")
        colourBy <- input$colour
        if(colourBy %in% weda::vba_name_conversions[["common_name"]]) {
          pa_data <- presence_absence_ldf %>%
            dplyr::filter(.data$common_name == !!colourBy) %>%
            dplyr::collect()

          map_data <- res_filter$filtered() %>%
            dplyr::left_join(pa_data, by = c("ProjectShortName", "SiteID", "SubStation", "Iteration"))

          if(input$removeNA) {
            map_data <- map_data[!is.na(map_data[["Presence"]]),]
          }

        } else {
          map_data <- res_filter$filtered()
        }

        if (colourBy == "ProjectName") {
          # the values are categorical
          pal <- leaflet::colorFactor("PuOr", map_data[[colourBy]])
          col_col <- colourBy
        } else {
          pal <- leaflet::colorFactor("RdYlBu", map_data[["Presence"]], na.color = "#e0e0e0")
          col_col <- "Presence"
        }

        shinycssloaders::hidePageSpinner()

        leaflet::leafletProxy("map") %>%
          leaflet::clearMarkers() %>%
          leaflet::removeControl("legend") %>%
          leaflet::clearShapes() %>%
          leaflet::setView(lng = 145, lat = -37, zoom = 6) %>%
          leaflet::addCircleMarkers(data = map_data, fillOpacity=0.8,
                                  fillColor=pal(map_data[[col_col]]), weight = 2, color = "black") %>%
          leaflet::addLegend("bottomright", pal=pal, values=map_data[[col_col]], title=col_col, layerId = "legend")

      })}
  )
}
