dataUploadpUI <- function(id,
                         label = "dataUpload") {
  ns <- shiny::NS(id)
  shiny::tabPanel("Data Upload",
                  sidebarLayout(
                    sidebarPanel = sidebarPanel(
                      shiny::tags$h4("Step 1"),
                      actionButton(inputId = ns("RecordButton"),
                                   label = "Upload Camera Records",
                                   icon = icon("upload"), width = "100%"),
                      width = 2),
                    mainPanel = mainPanel(htmlOutput(outputId = ns("step1")),
                                          width = 10)
                  ))



}

dataUploadServer <- function(id) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      myModal <- function() {
        ns <- session$ns
        modalDialog(import_ui(ns("UploadRecords"), from = "file"))
      }

      observeEvent(input$RecordButton, {
        showModal(myModal())
      })



      recs <- datamods::import_server("UploadRecords", return_class = "tbl_df")

      output$step1 <- renderText({
        req(recs$data())
        "&#10003; Step 1 Complete"
      })
})
}
