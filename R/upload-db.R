#' Prepare camera trap data for upload to the database
#'
#' This function takes a list of agents and prepares the data for upload to the database.
#'
#' @param agent_list A list of pointblank agents (directly from camera_trap_dq())
#' @return A list of data frames
#' @export
#' @examples
#' \dontrun{
#' prepare_upload(agent_list)
#' }
prepare_camtrap_upload <- function(agent_list) {

  if(!all(sapply(agent_list, function(x) all(x[["validation_set"]][["all_passed"]])))) {
    stop("Not all data quality checks have passed, please amend data before uploading to the database")
  }

  # Get three tables
  project_information <- agent_list[["project_information"]][["tbl"]] %>%
    dplyr::rowwise() %>%
    dplyr::mutate(camtrap_project_database_ID = digest::digest(paste0(.data$ProjectName), algo = "md5")) %>%
    dplyr::ungroup()

  camtrap_records <- agent_list[["camtrap_records"]][["tbl"]] %>%
    dplyr::mutate(ProjectShortName = project_information$ProjectShortName) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(camtrap_record_database_ID = digest::digest(paste0(.data$ProjectShortName, .data$SiteID, .data$SubStation, .data$DateTimeOriginal, .data$FileName), algo = "md5")) %>%
    dplyr::ungroup() %>%
    dplyr::select(ProjectShortName, everything())

  camtrap_operation <- agent_list[["camtrap_operation"]][["tbl"]]

  # Add project info to camtrap lists
  camtrap_operation$ProjectShortName <- project_information$ProjectShortName
  camtrap_operation$geohash <- geohashTools::gh_encode(latitude = camtrap_operation$Latitude,
                                                       longitude = camtrap_operation$Longitude,
                                                       precision = 8L)

  camtrap_operation <- camtrap_operation %>%
    dplyr::rowwise() %>%
    dplyr::mutate(camtrap_operation_database_ID = digest::digest(paste0(.data$ProjectShortName, .data$SiteID, .data$SubStation, .data$Iteration), algo = "md5")) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$ProjectShortName, .data$SiteID, .data$SubStation, .data$geohash, dplyr::everything())

  return(list(camtrap_records = camtrap_records,
              camtrap_operation = camtrap_operation,
              project_information = project_information))
}


#' Upload camera trap data to database
#'
#' @param con postgresql connection to ari-dev-weda-psql-01
#' @param data_list list of camera trap records, operations and project information (output from prepare_camtrap_upload())
#' @param uploadername name of person uploading data
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' upload_camtrap_data(con = con_odbc, data_list = data_list, uploadername = "Justin Cally")
#' }
upload_camtrap_data <- function(con, data_list, uploadername) {

  timestamp <- Sys.time()

  data_list <- lapply(data_list, function(x) {
    x$Timestamp <- timestamp
    x$Uploader <- uploadername
    return(x)
  })

  # Append record table
  DBI::dbWriteTable(con, DBI::Id(schema = "camtrap", table = "raw_camtrap_records"),
                    data_list[["camtrap_records"]], row.names = FALSE, append = TRUE, overwrite = FALSE)

  DBI::dbWriteTable(con, DBI::Id(schema = "camtrap", table = "raw_camtrap_operation"),
                    data_list[["camtrap_operation"]], row.names = FALSE, append = TRUE, overwrite = FALSE)

  DBI::dbWriteTable(con, DBI::Id(schema = "camtrap", table = "raw_project_information"),
                    data_list[["project_information"]], row.names = FALSE, append = TRUE, overwrite = FALSE)
}
