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
                  sidebarLayout(
                    sidebarPanel = sidebarPanel(
                      shiny::tags$h4("Step 1"), htmlOutput(outputId = ns("step1tick")),
                      actionButton(inputId = ns("generateCode"),
                                   label = "Generate camtrapR code",
                                   icon = icon("code"), width = "100%"),
                      shiny::tags$h4("Step 2"),
                      htmlOutput(outputId = ns("step2")),
                      actionButton(inputId = ns("RecordButton"),
                                   label = "Upload Camera Records",
                                   icon = icon("upload"), width = "100%"),
                      shiny::tags$h4("Step 3"),
                      htmlOutput(outputId = ns("step3")),
                      actionButton(inputId = ns("OperationButton"),
                                   label = "Upload Camera Operation",
                                   icon = icon("upload"), width = "100%"),
                      shiny::tags$h4("Step 4"),
                      htmlOutput(outputId = ns("step4")),
                      actionButton(inputId = ns("ProjectButton"),
                                   label = "Upload Project Information",
                                   icon = icon("upload"), width = "100%"),
                      shiny::tags$h4("Step 5"),
                      htmlOutput(outputId = ns("step5")),
                      shinyWidgets::radioGroupButtons(
                        inputId = ns("nameformat"),
                        label = "Species Name Format",
                        choices = c("scientific", "common"),
                        size = "sm",
                        status = "primary", width = "45%"),
                      shiny::textInput(inputId = ns("speciescol"),
                                       label = "Column name of species",
                                       value = "Species", width = "45%"),
                      actionButton(inputId = ns("standardise"),
                                   label = "Standardise Species Names",
                                   icon = icon("equals"), width = "100%"),
                      shiny::tags$h4("Step 6"),
                      htmlOutput(outputId = ns("step6")),
                      actionButton(inputId = ns("convertlatlong"),
                                   label = "Standardise Site Coordinates (e.g. 54/55 to lat/long)",
                                   icon = icon("map-pin"), width = "100%"),
                      shiny::tags$h4("Step 7"),
                      htmlOutput(outputId = ns("step7")),
                      actionButton(inputId = ns("viewsites"),
                                   label = "Show Map of Camera Sites",
                                   icon = icon("map"), width = "100%"),
                      shiny::tags$h4("Step 8"),
                      htmlOutput(outputId = ns("step8")),
                      actionButton(inputId = ns("dataquality"),
                                   label = "Run Data Quality",
                                   icon = icon("user-secret"), width = "100%"),
                      width = 3),
                    mainPanel = mainPanel(verbatimTextOutput(outputId = ns("step1")),
                                          htmlOutput(outputId = ns("standardisecli")),
                                          htmlOutput(outputId = ns("convertmessage")),
                                          leaflet::leafletOutput(outputId = ns("sitemap")),
                                          gt::gt_output(outputId = ns("dq1")),
                                          gt::gt_output(outputId = ns("dq2")),
                                          gt::gt_output(outputId = ns("dq3")),
                                          width = 9)
                  ))



}

#' @rdname dataUploadpUI
#' @export
dataUploadServer <- function(id) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      #### Step 1 ####
      stp1_code <- shiny::eventReactive(input$generateCode, {
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

                    saveRDS(raw_camtrap_records, "raw_camtrap_records.rds")'
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
        modalDialog(import_ui(ns("UploadRecords"), from = "file"))
      }

      shiny::observeEvent(input$RecordButton, {
        showModal(step2mod())
      })

      recs <- datamods::import_server("UploadRecords", return_class = "tbl_df")

      output$step2 <- renderText({
        req(recs$data())
        "&#10003; Step 2 Complete"
      })

      #### Step 3 ####

      step3mod <- function() {
        ns <- session$ns
        modalDialog(import_ui(ns("UploadOperation"), from = "file"))
      }

      shiny::observeEvent(input$OperationButton, {
        showModal(step3mod())
      })

      opers <- datamods::import_server("UploadOperation", return_class = "tbl_df")

      output$step3 <- renderText({
        req(opers$data())
        "&#10003; Step 3 Complete"
      })

      #### Step 4 ####

      step4mod <- function() {
        ns <- session$ns
        modalDialog(import_ui(ns("UploadProject"), from = "file"))
      }

      shiny::observeEvent(input$ProjectButton, {
        showModal(step4mod())
      })

      projs <- datamods::import_server("UploadProject", return_class = "tbl_df")

      output$step4 <- renderText({
        req(projs$data())
        "&#10003; Step 4 Complete"
      })

      #### Step 5 ####

      st_data <- shiny::eventReactive(input$standardise, {

        standardise_species_names2(
            recordTable = recs$data(),
            format = input$nameformat,
            speciesCol = input$speciescol,
            return_data = TRUE)

      })

      output$standardisecli <- shiny::renderText({
        req(st_data())

        op <- st_data()
        paste("Camera Trap Record Names have been standardised according to the VBA taxonomy.
              Check the conversions below, if there are species without conversions then you need to ammend your data
              by either filtering out records of non-taxa (e.g. 'person') or changing the names in the data to match VBA conventions. <br>",
        paste(cli::ansi_html(op[["messages"]]), collapse = "<br>"))
      })

      output$step5 <- renderText({
        req(st_data())
        "&#10003; Step 5 Complete"
      })

      #### Step 6 ####

      opers2 <- shiny::eventReactive(input$convertlatlong, {

        fixed_coords <- convert_to_latlong2(data = opers$data())
        return(fixed_coords)
      })

      output$step6 <- renderText({
        req(opers2())
        "&#10003; Step 6 Complete"
      })

      output$convertmessage <-  shiny::renderText({
        req(opers2())

        op2 <- opers2()

        return(cli::ansi_html(op2[["messages"]]))

      })

      #### Step 7 ####

      observeEvent(input$viewsites, {

      output$sitemap <- leaflet::renderLeaflet({
        camtrap_operation_mapview(opers2()$result)
      })

      output$step7 <- renderText({
        "&#10003; Step 7 Complete"
      })
   })

      #### Step 8 ####

      dqlist <- eventReactive(input$dataquality, {

        output$step8 <- renderText({
          "&#10003; Step 8 Complete"
        })

        withProgress(message = 'Running Data Quality', value = 0.5, {
        camera_trap_dq(camtrap_records = st_data()$result,
                                            camtrap_operation = opers2()$result,
                                            project_information = projs$data())
        })
      })

        output$dq1 <- gt::render_gt({
          req(dqlist())
          dqlist()[[1]]
        })

        output$dq2 <- gt::render_gt({
          req(dqlist())
          dqlist()[[2]]
        })

        output$dq3 <- gt::render_gt({
          req(dqlist())
          dqlist()[[3]]
        })

})
}
