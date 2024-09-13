#' Prepare transect data for upload to the database
#'
#' This function takes a list of agents and prepares the data for upload to the database.
#'
#' @param agent_list A list of pointblank agents (directly from transect_dq())
#' @return A list of data frames
#' @export
#' @examples
#' \dontrun{
#' prepare_transect_upload(agent_list)
#' }
prepare_transect_upload <- function(agent_list) {

  dq_check <- !is.null(agent_list) && all(sapply(agent_list, function(x) all(!x[["validation_set"]][["stop"]])))

  if(!dq_check) {
    stop("Not all data quality checks have passed, please amend data before uploading to the database")
  }

  # Get three tables
  project_information <- agent_list[["project_information"]][["tbl"]] %>%
    dplyr::rowwise() %>%
    dplyr::mutate(transect_project_database_ID = digest::digest(paste0(.data$ProjectName), algo = "md5")) %>%
    dplyr::ungroup()

  records <- agent_list[["records"]][["tbl"]] %>%
    dplyr::mutate(ProjectShortName = project_information$ProjectShortName) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(transect_record_database_ID = digest::digest(paste0(.data$ProjectShortName, .data$SiteID, .data$Transect, .data$Iteration, .data$ObserverPosition, .data$common_name, .data$AnimalID), algo = "md5")) %>%
    dplyr::ungroup() %>%
    dplyr::select(ProjectShortName, everything())

  transects <- agent_list[["transects"]][["tbl"]]

  # Add project info to camtrap lists
  transects$ProjectShortName <- project_information$ProjectShortName

  transects <- transects %>%
    dplyr::rowwise() %>%
    dplyr::mutate(transect_database_ID = digest::digest(paste0(.data$ProjectShortName, .data$SiteID, .data$Transect, .data$Iteration, .data$ObserverPosition), algo = "md5")) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$ProjectShortName, .data$SiteID, .data$Transect, .data$ObserverPosition, dplyr::everything())

  return(list(records = records,
              transects = transects,
              project_information = project_information))
}


#' Upload camera trap data to database
#'
#' @param con postgresql connection to ari-dev-weda-psql-01
#' @param data_list list of transect records, transects and project information (output from prepare_transect_upload())
#' @param uploadername name of person uploading data
#' @param tables_to_upload vector (characters) of tables to upload. Default is all of them
#' @param schema schema to upload data to (options are transect or transect_dev)
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' con <- weda_connect(password = keyring::key_get(service = "ari-dev-weda-psql-01",
#' username = "psql_user"))
#' upload_transect_data(con = con, data_list = data_list, uploadername = "Justin Cally")
#' }
upload_transect_data <- function(con,
                                data_list,
                                uploadername,
                                tables_to_upload = c("raw_transect_records",
                                                     "raw_transects",
                                                     "raw_project_information"),
                                schema = "transect") {

  timestamp <- Sys.time()

  data_list <- lapply(data_list, function(x) {
    x$Timestamp <- timestamp
    x$Uploader <- uploadername
    return(x)
  })

  # Append record table
  if("raw_transect_records" %in% tables_to_upload) {
    DBI::dbWriteTable(con, DBI::Id(schema = schema, table = "raw_transect_records"),
                      data_list[["records"]], row.names = FALSE, append = TRUE, overwrite = FALSE)
    message("Uploaded transect records")
    # DBI::dbExecute(conn = con, statement = "REFRESH MATERIALIZED VIEW camtrap.curated_camtrap_records;")
    # message("Refreshed records materialized VIEW")
  }

  if("raw_transects" %in% tables_to_upload) {
    DBI::dbWriteTable(con, DBI::Id(schema = schema, table = "raw_transects"),
                      data_list[["transects"]], row.names = FALSE, append = TRUE, overwrite = FALSE)
    message("Uploaded transect details")
    # DBI::dbExecute(conn = con, statement = "REFRESH MATERIALIZED VIEW camtrap.curated_camtrap_operation;")
    # message("Refreshed operation materialized VIEW")
  }

  if("raw_project_information" %in% tables_to_upload) {
    DBI::dbWriteTable(con, DBI::Id(schema = schema, table = "raw_project_information"),
                      data_list[["project_information"]], row.names = FALSE, append = TRUE, overwrite = FALSE)
    message("Uploaded transect project information")
    # DBI::dbExecute(conn = con, statement = "REFRESH MATERIALIZED VIEW camtrap.curated_project_information;")
    # message("Refreshed project info materialized VIEW")
  }

  # if(any(c("raw_camtrap_operation", "raw_camtrap_records") %in% tables_to_upload)) {
  #   DBI::dbExecute(conn = con, statement = "REFRESH MATERIALIZED VIEW camtrap.processed_site_substation_presence_absence;")
  #   message("Refreshed presence-absence materialized VIEW")
  # }

}
