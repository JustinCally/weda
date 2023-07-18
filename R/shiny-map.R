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

      res_filter <- datamods::filter_data_server(
        "project",
        data = shiny::reactive(project_locations),
        vars = shiny::reactive(c("ProjectName", "BaitedUnbaited",
                                 "BaitType", "DistanceSampling",
                          "AllSpeciesTagged", "ProjectStart",
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
          pa_data <- weda::processed_SubStation_presence_absence(con = con,
                                                           return_data = TRUE,
                                                           daily = FALSE,
                                                           species = colourBy)

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
          pal <- leaflet::colorFactor("RdYlBu", map_data[[colourBy]])
          col_col <- colourBy
        } else {
          pal <- leaflet::colorFactor(c("#00B2A9", "#201547"),
                                      map_data[["Presence"]],
                                      na.color = "#e0e0e0")
          col_col <- "Presence"
        }

        labels <- list()
        for(i in 1:nrow(map_data)) {

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

        shinycssloaders::hidePageSpinner()

        leaflet::leafletProxy("map") %>%
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
                                    direction = "auto")) %>%
          leaflet::addLegend("bottomright", pal=pal, values=map_data[[col_col]], title=col_col, layerId = "legend")

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
