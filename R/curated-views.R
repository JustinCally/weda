#' Curated views
#' @description Take raw records and return the most recently modified row for each record
#'
#' @param con database connection
#' @param return_data logical flag to return data (TRUE) or sql query (default is FALSE)
#'
#' @return sql or data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' con <- weda_connect(password = keyring::key_get(service = "ari-dev-weda-psql-01",
#' username = "psql_user"))
#' DBI::dbExecute(conn = con,
#'                paste(DBI::SQL("CREATE VIEW camtrap.curated_camtrap_records AS"),
#'                records_curated_view(con)))
#' DBI::dbExecute(conn = con,
#'                paste(DBI::SQL("CREATE VIEW camtrap.curated_camtrap_operation AS"),
#'                operation_curated_view(con)))
#' DBI::dbExecute(conn = con,
#'                paste(DBI::SQL("CREATE VIEW camtrap.curated_project_information AS"),
#'                project_curated_view(con)))
#' }
records_curated_view <- function(con, return_data = FALSE) {

  # load the raw_camtrap_records table from the camtrap schema
  raw_camtrap_records <- dplyr::tbl(con, dbplyr::in_schema("camtrap", "raw_camtrap_records"))

  # group the raw_camtrap_records table by camtrap_record_database_ID
  # and filter the table to only include the most recent record for each camtrap_record_database_ID
  curated_camtrap_records <- raw_camtrap_records %>%
    dplyr::select(-n_images) %>%
    dplyr::distinct() %>%
    dplyr::group_by(camtrap_record_database_ID) %>%
    dplyr::filter(Timestamp == max(Timestamp, na.rm = TRUE))

  if(return_data) {
    return(dplyr::collect(curated_camtrap_records))
  } else {
    return(dbplyr::remote_query(curated_camtrap_records))
  }
}

#' @rdname records_curated_view
#' @export
operation_curated_view <- function(con, return_data = FALSE) {

  raw_camtrap_operation <- dplyr::tbl(con, dbplyr::in_schema("camtrap", "raw_camtrap_operation"))

  curated_camtrap_operation <- raw_camtrap_operation %>%
    dplyr::group_by(camtrap_operation_database_ID) %>%
    dplyr::filter(Timestamp == max(Timestamp, na.rm = TRUE))

  if(return_data) {
    return(dplyr::collect(curated_camtrap_operation))
  } else {
    return(dbplyr::remote_query(curated_camtrap_operation))
  }
}

#' @rdname records_curated_view
#' @export
project_curated_view <- function(con, return_data = FALSE) {

  raw_project_information <- dplyr::tbl(con, dbplyr::in_schema("camtrap", "raw_project_information"))

  curated_project_information <- raw_project_information %>%
    dplyr::group_by(camtrap_project_database_ID) %>%
    dplyr::filter(Timestamp == max(Timestamp, na.rm = TRUE))

  if(return_data) {
    return(dplyr::collect(curated_project_information))
  } else {
    return(dbplyr::remote_query(curated_project_information))
  }
}
