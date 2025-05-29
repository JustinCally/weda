read_camtrap_csv_records <- function(df) {
  if ("Date" %in% names(df)) {
    if (is.numeric(df$Date)) {
      df$Date <- as.Date(df$Date, origin = "1899-12-30")
    } else if (is.character(df$Date)) {
      df$Date <- suppressWarnings(lubridate::dmy(df$Date))
    }
  }

  if ("DateTimeOriginal" %in% names(df)) {
    if (is.numeric(df$DateTimeOriginal)) {
      df$DateTimeOriginal <- as.POSIXct(df$DateTimeOriginal * 86400, origin = "1899-12-30", tz = "UTC")
    } else if (is.character(df$DateTimeOriginal)) {
      df$DateTimeOriginal <- suppressWarnings(lubridate::dmy_hm(df$DateTimeOriginal))
    }
  }

  return(df)
}

read_camtrap_csv_operation <- function(df) {
  date_fields <- c("DateDeploy", "DateRetrieve")
  datetime_fields <- c("DateTimeDeploy", "DateTimeRetrieve")

  for (col in date_fields) {
    if (col %in% names(df)) {
      if (is.numeric(df[[col]])) {
        df[[col]] <- as.Date(df[[col]], origin = "1899-12-30")
      } else if (is.character(df[[col]])) {
        df[[col]] <- suppressWarnings(lubridate::dmy(df[[col]]))
      }
    }
  }

  for (col in datetime_fields) {
    if (col %in% names(df)) {
      if (is.numeric(df[[col]])) {
        df[[col]] <- as.POSIXct(df[[col]] * 86400, origin = "1899-12-30", tz = "UTC")
      } else if (is.character(df[[col]])) {
        df[[col]] <- suppressWarnings(lubridate::dmy_hm(df[[col]]))
      }
    }
  }

  return(df)
}


warn_suspicious_dates <- function(dates, context = "dates", min_year = 2022) {
  if (!is.null(dates) && any(!is.na(dates) & lubridate::year(dates) < min_year)) {
    shiny::showNotification(
      paste("Some", context, "appear to be before", min_year, ". This may indicate Excel date conversion issues."),
      type = "warning", duration = 8
    )
  }
}

#' capture messages
#'
#' @param fun function to wrap
#'
#' @noRd
#'
#' @return results and messages
capture_cli_messages <- function(fun) {

  function(..., .quiet = TRUE) {

    output <- list(result = NULL, messages = NULL)

    output$messages <- cli::cli_fmt({
      output$result <- fun(...)
    })

    if (!.quiet) cat(output$messages, sep = "\n")

    output

  }
}

standardise_species_names2 <- capture_cli_messages(weda::standardise_species_names)
convert_to_latlong2 <- capture_cli_messages(weda::convert_to_latlong)
camera_trap_dq2 <- capture_cli_messages(weda::camera_trap_dq)

#' Shiny help popup
#'
#' @param title title
#' @param content content
#' @param placement placement
#' @param trigger trigger
#'
#' @noRd
#'
#' @return shinyhtml
helpPopup <- function(title = NULL, content,
                      placement=c('right', 'top', 'left', 'bottom'),
                      trigger=c('click', 'hover', 'focus', 'manual')) {
  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(
        shiny::tags$style(shiny::HTML(".popover{
    max-width: 600px;
    container: 'body'}")),
        shiny::tags$script("$(function() { $(\"[data-toggle='popover']\").popover(); })")
      )
    ),
    shiny::tags$a(
      href = "#", class = "btn btn-mini", `data-toggle` = "popover", style="padding: 0px 5px 5px 0px;display:inline-block",
      title = title, `data-content` = shiny::HTML(as.character(content)), `data-animation` = TRUE, `data-html`="true",
      `data-placement` = match.arg(placement, several.ok=TRUE)[1],
      `data-trigger` = match.arg(trigger, several.ok=TRUE)[1],

      shiny::icon("question")
    )
  )
}


#' Data Upload Shiny Module
#'
#' @param id shiny samespace id
#' @param label label of UI
#'
#' @return shiny module
#' @export
dataUploadpUI <- function(id,
                         label = "dataUpload") {

  shinyjs::useShinyjs()
  ns <- shiny::NS(id)
  shiny::tabPanel("Data Upload",
                  shiny::sidebarLayout(
                    shiny::sidebarPanel(
                      shinyWidgets::downloadBttn(
                        outputId = ns("downloadSample"),style = "bordered", label = "Download Example Data", size = "xs", color = "primary"),
                      shiny::br(),
                      shiny::div(shiny::tags$h4("Step 1", style="display:inline-block"),
                                 helpPopup(title = "Step 1 Guide", content = "This step shows R code that can be used to obtain a
                                           tabularised record of the metadata from the camera trap images. It is based on the
                                           recordTable() function from the camtrapR package. Users should use this function within an
                                           interactive R session. If there are lots of images this function can take a bit of time and
                                           thus it is not automated in this shiny app.")),
                      shiny::htmlOutput(outputId = ns("step1tick")),
                      shiny::actionButton(inputId = ns("generateCode"),
                                          label = "Generate camtrapR code",
                                          icon = shiny::icon("code"), width = "100%"),
                      shiny::div(shiny::tags$h4("Step 2", style="display:inline-block"),
                                 helpPopup(title = "Step 2 Guide", content = "Upload the camera trap records generated in step 1.
                                           Make sure the data has all the columns in the example data template (download above)")),
                      shiny::htmlOutput(outputId = ns("step2")),
                      shiny::actionButton(inputId = ns("RecordButton"),
                                   label = "Import Camera Records",
                                   icon = shiny::icon("upload"), width = "100%"),
                      shiny::div(shiny::tags$h4("Step 3", style="display:inline-block"),
                                 helpPopup(title = "Step 3 Guide", content = "Upload the camera trap operation data (generated from proofsafe or manually from field data).
                                           Make sure the data has all the columns in the example data template (download above)")),
                      shiny::htmlOutput(outputId = ns("step3")),
                      shiny::actionButton(inputId = ns("OperationButton"),
                                   label = "Import Camera Operation",
                                   icon = shiny::icon("upload"), width = "100%"),
                      shiny::div(shiny::tags$h4("Step 4", style="display:inline-block"),
                                 helpPopup(title = "Step 4 Guide", content = "Upload the camera trap project information.
                                           Make sure the data has all the columns in the example data template (download above)")),
                      shiny::htmlOutput(outputId = ns("step4")),
                      shiny::actionButton(inputId = ns("ProjectButton"),
                                   label = "Import Project Information",
                                   icon = shiny::icon("upload"), width = "100%"),
                      shiny::div(shiny::tags$h4("Step 5", style="display:inline-block"),
                                 helpPopup(title = "Step 5 Guide", content = "This step standardises species names (scientific to common or vice versa).
                                           Choose the format that you tagged the species names in (scientific or common) and the name of the column with
                                           species name (default is 'Species'). If some conversions are not possible they will also be tagged in Step 8.
                                           The database only accepts species listed in the VBA.")),
                      shiny::htmlOutput(outputId = ns("step5")),
                      shinyWidgets::radioGroupButtons(
                        inputId = ns("nameformat"),
                        label = "Species Name Format",
                        choices = c("scientific", "common"),
                        size = "sm",
                        status = "primary", width = "45%"),
                      shiny::textInput(inputId = ns("speciescol"),
                                       label = "Column name of species",
                                       value = "Species", width = "45%"),
                      shiny::actionButton(inputId = ns("standardise"),
                                   label = "Standardise Species Names",
                                   icon = shiny::icon("equals"), width = "100%"),
                      shiny::div(shiny::tags$h4("Step 6", style="display:inline-block"),
                                 helpPopup(title = "Step 6 Guide", content = "This step checks and standardises the coordinates in the operation data.
                                           It will confirm that you have Latitude and Longitude fields. If you have an 'Easting', 'Northing' and 'Zone' fields
                                           instead, it will convert these to 'Latitude' and 'Longitude'")),
                      shiny::htmlOutput(outputId = ns("step6")),
                      shiny::actionButton(inputId = ns("convertlatlong"),
                                   label = "Standardise Site Coords (e.g. 54/55 to lat/long)",
                                   icon = shiny::icon("map-pin"), width = "100%"),
                      shiny::div(shiny::tags$h4("Step 7", style="display:inline-block"),
                                 helpPopup(title = "Step 7 Guide", content = "Map showing the sites (with SiteID). Check that they are correct.")),
                      shiny::htmlOutput(outputId = ns("step7")),
                      shiny::actionButton(inputId = ns("viewsites"),
                                   label = "Show Map of Camera Sites",
                                   icon = shiny::icon("map"), width = "100%"),
                      shiny::div(shiny::tags$h4("Step 8", style="display:inline-block"),
                                 helpPopup(title = "Step 8 Guide", content = shiny::div(shiny::tags$h5("This step may take some time to run.
                                           It conducts 100 data checks to make sure there are no issues in the quality of your data.
                                           All data checks need to pass for uploads to succeed."),
                                                                                   shiny::tags$ul(
                                                                                     shiny::tags$li("The 'STEP' column outlines the type of check performed. Hover over it for more detail"),
                                                                                     shiny::tags$li("The 'COLUMNS' column outlines the columns the check was performed on. Some columns are newly created ones in order to investigate the effect of a derived/generated data value"),
                                                                                     shiny::tags$li("The 'TBL' column shows whether some data modification took place in order to generate a derived variable"),
                                                                                     shiny::tags$li("The 'EVAL' column shows whether the analysis was completed"),
                                                                                     shiny::tags$li("The 'UNITS' column shows how many data points were evaluated (number of rows)"),
                                                                                     shiny::tags$li("The 'PASS' column outlines the number of units passing the checks (and the proportion)"),
                                                                                     shiny::tags$li("The 'FAIL' column outlines the number of units failing the checks (and the proportion)"),
                                                                                     shiny::tags$li("The 'W' column is a 'WARNING' that the data quality should be checked and investigated further. A warning (yellow dot), does not mean the analysis is not possible. However, fixing the data for a warning can help maximise the usability and value of the data, albeit not essential. Sometimes this warning is just present in order to double-check odd results."),
                                                                                     shiny::tags$li("The 'S' column is a 'STOP' indicator. This means that the data should not be used for this data entry. In order for us to use that data without further cleaning and guesswork it should be fixed"),
                                                                                     shiny::tags$li("The 'N' column 'NOTIFYS' the user of an odd (but not unusable) concern with the data such as high counts or odd patterns. Double check that this data is accurate"),
                                                                                     shiny::tags$li(shiny::HTML(paste0("The colour of the filled dot indicates which of the above three conditions were met. ", shiny::strong("Ultimately, if there are no red dots the data is usable")))),
                                                                                     shiny::tags$li(shiny::HTML(paste0("The 'EXT' column is a button to download the failing rows of the data. Download this CSV to help find which rows need to be fixed. ", shiny::strong("Do not edit the CSV, instead use it to find the rows in the original data (on sharepoint) to clean/fix/ammend"))))
                                                                                   )))),
                      shiny::htmlOutput(outputId = ns("step8")),
                      shiny::actionButton(inputId = ns("dataquality"),
                                   label = "Run Data Quality",
                                   icon = shiny::icon("user-secret"), width = "100%"),
                      shiny::div(shiny::tags$h4("Step 9", style="display:inline-block"),
                                 helpPopup(title = "Step 9 Guide", content = "Upload the data. Data must pass all previous steps (including no 'Stops' in the data quality).
                                           You must enter your name and confirm the upload. If data is successfully uploaded you will receive a message in the main panel.
                                           Note this step may take some time if data is large. Please be patient.")),
                      shiny::htmlOutput(outputId = ns("step9")),
                      shiny::actionButton(inputId = ns("uploaddata"),
                                   label = "Upload to Database",
                                   icon = shiny::icon("database"), width = "100%"),
                      width = 3),
                    mainPanel = shiny::mainPanel(shinyBS::bsCollapse(id = ns("collapsepanel"), multiple = FALSE,
                                            shinyBS::bsCollapsePanel(title = "Step 1 Output",
                                                                     shiny::verbatimTextOutput(outputId = ns("step1"))),
                                            shinyBS::bsCollapsePanel(title = "Step 5 Output",
                                                                     shiny::htmlOutput(outputId = ns("standardisecli"))),
                                            shinyBS::bsCollapsePanel(title = "Step 6 Output",
                                                                     shiny::htmlOutput(outputId = ns("convertmessage"))),
                                            shinyBS::bsCollapsePanel(title = "Step 7 Output",
                                                                       shiny::uiOutput(ns("species_selector")),  # dynamic UI
                                                                   leaflet::leafletOutput(outputId = ns("sitemap"))),
                                            shinyBS::bsCollapsePanel(title = "Step 8 Output",
                                                                     shiny::htmlOutput(outputId = ns("dqmessages")),
                                                                   gt::gt_output(outputId = ns("dq1")),
                                                                   gt::gt_output(outputId = ns("dq2")),
                                                                   gt::gt_output(outputId = ns("dq3"))),
                                          shinyBS::bsCollapsePanel(title = "Step 9 Output",
                                                                   shiny::htmlOutput(outputId = ns("uploadcompletion")),
                                                                   shinyWidgets::downloadBttn(
                                                                     outputId = ns("downloadVBA"),
                                                                     style = "bordered",
                                                                     label = "Download VBA Data",
                                                                     size = "sm",
                                                                     color = "primary"))),
                                          width = 9)
                  ))



}

#' @rdname dataUploadpUI
#' @param con database connection
#' @export
dataUploadServer <- function(id, con) {

  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$downloadSample <- shiny::downloadHandler(
        filename <- function() {
          "camera_trap_templates.xlsx"
        },

        content <- function(file) {
          file.copy(system.file("app/www/camera_trap_templates.xlsx", package = "weda"), file)
        },
        contentType = "xlsx"
      )

      #### Step 1 ####
      stp1_code <- shiny::eventReactive(input$generateCode, {
        shinyBS::updateCollapse(session = session, id = "collapsepanel", open = "Step 1 Output")

' # For you to upload camera trap data to the database tagged images need to have metadata extracted from them.
 # To extract metadata from the images use the camtrapR package to generate a table of all records. Below is an
 # example you can try running. If there are many images this process might take a long time so it is
 # best to save the resulting table (e.g. raw_camtrap_records) as an rds, excel or csv which you can then upload in step 2.

        raw_camtrap_records <- camtrapR::recordTable(inDir  = YOUR_IMAGE_PATH,
                               IDfrom = "metadata",
                               cameraID = "directory",
                               stationCol = "SiteID",
                               camerasIndependent = TRUE,
                               timeZone = Sys.timezone(location = TRUE),
                               metadataSpeciesTag = "Species",
                               removeDuplicateRecords = FALSE,
                               returnFileNamesMissingTags = TRUE)
  # Save as an rds object (good for R usage)
                    saveRDS(raw_camtrap_records, "raw_camtrap_records.rds")
  # Save as csv (good for viewing in excel)
                    write.csv(raw_camtrap_records, "raw_camtrap_records.csv")'
      })

      output$step1 <- shiny::renderText({
        stp1_code()
      })

      output$step1tick <- shiny::renderText({
        req(stp1_code())
        "&#10003; Step 1 Complete"
      })

      #### Step 2 ####
      step2mod <- function() {
        ns <- session$ns
        shiny::modalDialog(datamods::import_ui(ns("UploadRecords"), from = "file"))
      }

      shiny::observeEvent(input$RecordButton, {
        shiny::showModal(step2mod())
      })

      recs <- datamods::import_server("UploadRecords", return_class = "tbl_df")

      # Cleaned camera records
      recs_clean <- reactive({
        req(recs$data())
        read_camtrap_csv_records(recs$data())
      })

      observe({
        req(recs_clean())
        if ("Date" %in% names(recs_clean())) {
          warn_suspicious_dates(recs_clean()$Date, "camera record dates")
        }
      })

      output$step2 <- shiny::renderText({
        shiny::req(recs_clean())
        "&#10003; Step 2 Complete"
      })

      #### Step 3 ####

      step3mod <- function() {
        ns <- session$ns
        shiny::modalDialog(datamods::import_ui(ns("UploadOperation"), from = "file"))
      }

      shiny::observeEvent(input$OperationButton, {
        shiny::showModal(step3mod())
      })

      opers <- datamods::import_server("UploadOperation", return_class = "tbl_df")

      # Cleaned operation data
      opers_clean <- reactive({
        req(opers$data())
        read_camtrap_csv_operation(opers$data())
      })

      observe({
        req(opers_clean())
        if ("DateDeploy" %in% names(opers_clean())) {
          warn_suspicious_dates(opers_clean()$DateDeploy, "deployment dates")
        }
        if ("DateRetrieve" %in% names(opers_clean())) {
          warn_suspicious_dates(opers_clean()$DateRetrieve, "retrieval dates")
        }
      })

      output$step3 <- shiny::renderText({
        shiny::req(opers_clean())
        "&#10003; Step 3 Complete"
      })

      #### Step 4 ####

      step4mod <- function() {
        ns <- session$ns
        shiny::modalDialog(datamods::import_ui(ns("UploadProject"), from = "file"))
      }

      shiny::observeEvent(input$ProjectButton, {
        shiny::showModal(step4mod())
      })

      projs <- datamods::import_server("UploadProject", return_class = "tbl_df")

      output$step4 <- shiny::renderText({
        shiny::req(projs$data())
        "&#10003; Step 4 Complete"
      })

      #### Step 5 ####

      st_data <- shiny::eventReactive(input$standardise, {
        shinyBS::updateCollapse(session = session, id = "collapsepanel",
                                open = "Step 5 Output", close = "Step 1 Output")

        standardise_species_names2(
            recordTable = recs_clean(),
            format = input$nameformat,
            speciesCol = input$speciescol,
            return_data = TRUE)

      })

      output$standardisecli <- shiny::renderText({
        shiny::req(st_data())

        op <- st_data()
        paste("Camera Trap Record Names have been standardised according to the VBA taxonomy.
              Check the conversions below, if there are species without conversions then you need to ammend your data
              by either filtering out records of non-taxa (e.g. 'person') or changing the names in the data to match VBA conventions. <br>",
        paste(cli::ansi_html(op[["messages"]]), collapse = "<br>"))
      })

      output$step5 <- shiny::renderText({
        shiny::req(st_data())
        "&#10003; Step 5 Complete"
      })

      #### Step 6 ####

      opers2 <- shiny::eventReactive(input$convertlatlong, {
        shinyBS::updateCollapse(session = session, id = "collapsepanel", open = "Step 6 Output", close = "Step 5 Output")

        fixed_coords <- convert_to_latlong2(data = opers_clean())
        return(fixed_coords)
      })

      output$step6 <- shiny::renderText({
        shiny::req(opers2())
        "&#10003; Step 6 Complete"
      })

      output$convertmessage <-  shiny::renderText({
        shiny::req(opers2())

        op2 <- opers2()

        return(cli::ansi_html(op2[["messages"]]))

      })

      #### Step 7 ####

      # Generate UI for species selector
      output$species_selector <- shiny::renderUI({
        shiny::req(st_data()$result)
        species_choices <- unique(st_data()$result$common_name)
        species_choices <- sort(species_choices[!is.na(species_choices)])

        shiny::selectInput(ns("species"), "Select species to view on map:",
                           choices = species_choices,
                           selected = species_choices[1])
      })

      observeEvent(input$viewsites, {
        shinyBS::updateCollapse(session = session, id = "collapsepanel", open = "Step 7 Output", close = "Step 6 Output")

        output$sitemap <- leaflet::renderLeaflet({
          shiny::req(opers2()$result, st_data()$result, input$species)

          weda::camtrap_operation_mapview(
            camtrap_operation_table = opers2()$result,
            records = st_data()$result,
            species_name = input$species
          )
        })

      output$step7 <- shiny::renderText({
        "&#10003; Step 7 Complete"
      })
   })

      #### Step 8 ####

      shiny::observeEvent(input$dataquality, {
        shinyBS::updateCollapse(session = session, id = "collapsepanel",
                                open = "Step 8 Output", close = "Step 7 Output")
      })

      dqlist <- shiny::eventReactive(input$dataquality, {

        output$step8 <- shiny::renderText({
          "&#10003; Step 8 Complete"
        })

        shiny::withProgress(message = 'Running Data Quality', value = 0.5, {
        camera_trap_dq2(camtrap_records = st_data()$result,
                             camtrap_operation = opers2()$result,
                             project_information = projs$data())
        })
      })

      output$dqmessages <- shiny::renderText({
        shiny::req(dqlist())

        dqmess <- dqlist()

        return(cli::ansi_html(dqmess[["messages"]]))
      })

        output$dq1 <- gt::render_gt({
          shiny::req(dqlist()$result)
          dqlist()$result[[1]] %>%
            pointblank::get_agent_report(title = "Data Quality Assessment on Camera Trap Records")
        })

        output$dq2 <- gt::render_gt({
          shiny::req(dqlist()$result)
          dqlist()$result[[2]] %>%
            pointblank::get_agent_report(title = "Data Quality Assessment on Camera Operation Records")
        })

        output$dq3 <- gt::render_gt({
          shiny::req(dqlist()$result)
          dqlist()$result[[3]] %>%
            pointblank::get_agent_report(title = "Data Quality Assessment on Project Information")
        })

        #### Step 9 ####

        observeEvent(input$uploaddata, {
          shiny::req(dqlist()$result)
          shinyBS::updateCollapse(session = session, id = "collapsepanel",
                                  open = "Step 9 Output", close = "Step 8 Output")

          shinyalert::shinyalert(
            title = "Are you sure you want to upload?",
            inputType = "text",
            type = "input",
            inputId = "name",
            text = "Type your full name to upload data",
            inputPlaceholder = "Firstname Surname",
            size = "m",
            closeOnEsc = TRUE,
            closeOnClickOutside = FALSE,
            html = FALSE,
            showConfirmButton = TRUE,
            showCancelButton = TRUE,
            confirmButtonText = "Upload",
            confirmButtonCol = "#AEDEF4",
            cancelButtonText = "Cancel",
            timer = 0,
            imageUrl = "",
            animation = TRUE
          )
        })

        shiny::observe({
          # Only continue if filled out
          shiny::req(input$name)

          shiny::withProgress(message = 'Preparing Upload', value = 0.5, {
          data_for_upload <- weda::prepare_camtrap_upload(agent_list = dqlist()$result)

          shiny::incProgress(amount = 0.25, message = "Uploading Data")

          weda::upload_camtrap_data(con = con,
                              data_list = data_for_upload,
                              uploadername = input$name,
                              schema = "camtrap")

          output$uploadcompletion <- shiny::renderText({
            "Upload Complete. Restart app to see project data on map pane"
          })

          output$downloadVBA <- shiny::downloadHandler(
            filename = function() {
              paste('camtrap_vba_data_', Sys.Date(), '.csv', sep='')
            },
            content = function(dl_con) {
              vba_data <- vba_format(con = con,
                                     return_data = T,
                                     schema = "camtrap",
                                     ProjectShortName = data_for_upload[["project_information"]]$ProjectShortName)
              readr::write_csv(vba_data, dl_con)
            }
          )

          output$step9 <- shiny::renderText({
            "&#10003; Step 9 Complete"
          })

          })

        })

})
}
