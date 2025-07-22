#' Proofsafe to operation
#'
#' @description
#' Take the csv results file from proofsafe and format it into the operations table format.
#'
#'
#' @param data data.frame/tbl from reading in the proofsafe results csv
#' @param sensitivity Sensitivity of the camera (e.g. "High", or "Very High")
#' @param delay Settings of the camera delay (e.g. Rapidfire)
#' @param photos_per_trigger Settings of how many photos per
#' @param quiet_period How long the camera is delayed after each trigger (e.g. usually 0)
#'
#' @returns data.frame
#' @export
#' @examples
#' \dontrun{
#' test_op <- proofsafe_to_operation(data = data,
#'   sensitivity = "Very High",
#'.  delay = "Rapidfire",
#'.  photos_per_trigger = 5, quiet_period = 0)
#' }
#'
proofsafe_to_operation <- function(data,
                                   sensitivity,
                                   delay,
                                   photos_per_trigger,
                                   quiet_period) {

  formatted_deps <- data %>%
    dplyr::filter(I2_Stage == "Installation") %>%
    tidyr::separate_wider_delim(cols = "I2_CamLatLong_GEO", delim = ",", names = c("Latitude", "Longitude")) %>%
    dplyr::mutate(SiteID = I2_SiteID,
                  SubStation = I2_Substation,
                  Iteration = I2_Cam_Iteration,
                  Latitude,
                  Longitude,
                  DateDeploy = as.Date(H1_Date, format = "%m/%d/%Y"),
                  TimeDeploy = I2_Time,
                  Problem1_from = NA,
                  Problem1_to = NA,
                  DateTimeDeploy = as.POSIXct(paste(DateDeploy, TimeDeploy)),
                  CameraHeight = I2_CamHeight,
                  CameraBearing = I2_CamAspect,
                  CameraSlope = I2_TerrainSlope,
                  CameraID = I2_CameraID,
                  CameraModel = I2_CameraModel,
                  CameraSensitivity = sensitivity,
                  CameraDelay = delay,
                  CameraPhotosPerTrigger = photos_per_trigger,
                  CameraQuietPeriod = quiet_period,
                  BaitedUnbaited = dplyr::case_when(I2_SurveyType == "Baited" ~ "Baited",
                                                    TRUE ~ "Unbaited"),
                  BaitType = I2_BaitType,
                  BaitDistance = I2_CamBaitDist,
                  .keep = "none")

    formatted_rets <- data %>%
      dplyr::filter(I2_Stage == "Retrieval") %>%
    dplyr::mutate(SiteID = I2_SiteID,
                  SubStation = I2_Substation,
                  DateRetrieve = as.Date(H1_Date, format = "%m/%d/%Y"),
                  TimeRetrieve = I2_Time,
                  DateTimeRetrieve = as.POSIXct(paste(DateRetrieve, TimeRetrieve)),
                  .keep = "none")

    joined_ops <- formatted_deps %>%
      dplyr::left_join(formatted_rets, by = dplyr::join_by(SiteID, SubStation)) %>%
      dplyr::select(SiteID,
                    SubStation,
                    Iteration,
                    Latitude,
                    Longitude,
                    DateDeploy,
                    TimeDeploy,
                    DateRetrieve,
                    TimeRetrieve,
                    Problem1_from,
                    Problem1_to,
                    DateTimeDeploy,
                    DateTimeRetrieve,
                    CameraHeight,
                    CameraBearing,
                    CameraSlope,
                    CameraID,
                    CameraModel,
                    CameraSensitivity,
                    CameraDelay,
                    CameraPhotosPerTrigger,
                    CameraQuietPeriod,
                    BaitedUnbaited,
                    BaitType,
                    BaitDistance)

    return(joined_ops)
}

#' proofsafe formatter module
#'
#' @param id shiny id
#'
#' @returns shiny UI
#' @export
proofsafeDownloadUI <- function(id) {

  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::actionButton(ns("launch"), "Upload and Reformat Proofsafe Data"),
    shiny::downloadButton(ns("download_trigger"), label = NULL, style = "display: none;")
  )
}

#' @rdname proofsafeDownloadUI
#' @export
proofsafeDownloadServer <- function(id) {

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    processed_data <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$launch, {
      shiny::showModal(
        shiny::modalDialog(
          title = "Upload CSV and Reformat",
          shiny::fileInput(ns("file_upload"), "Choose CSV File", accept = ".csv"),
          shiny::textInput(ns("sensitivity"), "Camera Sensitivity", value = "High"),
          shiny::textInput(ns("delay"), "Camera Delay", value = "Rapidfire"),
          shiny::numericInput(ns("photos"), "Photos Per Trigger", value = 5, min = 1),
          shiny::numericInput(ns("quiet"), "Quiet Period", value = 0, min = 0),
          footer = shiny::tagList(
            shiny::downloadButton(ns("download_trigger"), "Process and Download"),
            shiny::modalButton("Cancel")
          ),
          easyClose = TRUE
        )
      )
    })

    reactive_processed <- shiny::reactive({
      shiny::req(input$file_upload)
      df <- readr::read_csv(input$file_upload$datapath, show_col_types = FALSE)
      proofsafe_to_operation(
        data = df,
        sensitivity = input$sensitivity,
        delay = input$delay,
        photos_per_trigger = input$photos,
        quiet_period = input$quiet
      )
    })

    output$download_trigger <- shiny::downloadHandler(
      filename = function() {
        paste0("formatted_operations_", Sys.Date(), ".csv")
      },
      content = function(file) {
        readr::write_csv(reactive_processed(), file, na = "")
        shiny::removeModal()
      }
    )
  })
}
