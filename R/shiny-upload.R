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
                      width = 3),
                    mainPanel = mainPanel(verbatimTextOutput(outputId = ns("step1")),
                                          htmlOutput(outputId = ns("standardisecli")),
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

        cat("In standardise 1")
        # req(input$nameformat)
        # req(input$speciescol)
        # req(recs$data())
        cat("In standardise 2")

        standardise_species_names2(
            recordTable = recs$data(),
            format = input$nameformat,
            speciesCol = input$speciescol,
            return_data = TRUE)

      })

      output$standardisecli <- renderText({
        req(st_data())

        op <- st_data()
        paste("Camera Trap Record Names have been standardised according to the VBA taxonomy.
              Check the conversions below, if there are species without conversions then you need to ammend your data
              by either filtering out records of non-taxa (e.g. 'person') or changing the names in the data to match VBA conventions. <br>",
        paste(cli::ansi_html(op[["messages"]]), collapse = "<br>"))
      })

})
}
