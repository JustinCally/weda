#' check whether project is unique or already on database
#'
#' @param ProjectShortName A list of pointblank agents (directly from camera_trap_dq())
#' @param con database connection
#'
#' @return message
#' @export
check_unique_project <- function(ProjectShortName, con) {
  # check project name in database
  pr_sn <- ProjectShortName

same_proj <- dplyr::tbl(con, dbplyr::in_schema("camtrap", table = "curated_project_information")) %>%
  dplyr::filter(ProjectShortName %in% !!pr_sn) %>%
  dplyr::collect()

if(nrow(same_proj) > 0) {
  message(paste0("Project already exists. Data will be appended onto the existing project (",
                 pr_sn,
                 ")"))
} else {
  message(paste0("Project does not exist yet on database. Upload will create a new project (",
                 pr_sn,
                 ")"))
}

}

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

  dq_check <- !is.null(agent_list) && all(sapply(agent_list, function(x) all(!x[["validation_set"]][["stop"]])))

  if(!dq_check) {
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
#' @param tables_to_upload vector (characters) of tables to upload. Default is all of them
#' @param schema schema to upload data to (options are camtrap or camtrap_dev)
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' con <- weda_connect(password = keyring::key_get(service = "ari-dev-weda-psql-01",
#' username = "psql_user"))
#' upload_camtrap_data(con = con, data_list = data_list, uploadername = "Justin Cally")
#' }
upload_camtrap_data <- function(con,
                                data_list,
                                uploadername,
                                tables_to_upload = c("raw_camtrap_records",
                                                     "raw_camtrap_operation",
                                                     "raw_project_information"),
                                schema = "camtrap") {

  timestamp <- Sys.time()

  data_list <- lapply(data_list, function(x) {
    x$Timestamp <- timestamp
    x$Uploader <- uploadername
    return(x)
  })

  # Append record table
  if("raw_camtrap_records" %in% tables_to_upload) {
  DBI::dbWriteTable(con, DBI::Id(schema = schema, table = "raw_camtrap_records"),
                    data_list[["camtrap_records"]], row.names = FALSE, append = TRUE, overwrite = FALSE)
    message("Uploaded camera trap records")
  }

  if("raw_camtrap_operation" %in% tables_to_upload) {
  DBI::dbWriteTable(con, DBI::Id(schema = schema, table = "raw_camtrap_operation"),
                    data_list[["camtrap_operation"]], row.names = FALSE, append = TRUE, overwrite = FALSE)
    message("Uploaded camera trap operation details")
  }

  if("raw_project_information" %in% tables_to_upload) {
  DBI::dbWriteTable(con, DBI::Id(schema = schema, table = "raw_project_information"),
                    data_list[["project_information"]], row.names = FALSE, append = TRUE, overwrite = FALSE)
    message("Uploaded camera trap project information")
  }
}
