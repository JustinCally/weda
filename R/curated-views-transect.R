#' @rdname records_curated_view
transect_records_curated_view <- function(con, return_data = FALSE) {

  # load the raw_camtrap_records table from the camtrap schema
  raw_transect_records <- dplyr::tbl(con, dbplyr::in_schema("transect", "raw_transect_records"))

  # group the raw_camtrap_records table by camtrap_record_database_ID
  # and filter the table to only include the most recent record for each camtrap_record_database_ID
  curated_transect_records <- raw_transect_records %>%
    dplyr::group_by(transect_record_database_ID) %>%
    dplyr::filter(Timestamp == max(Timestamp, na.rm = TRUE))

  if(return_data) {
    return(dplyr::collect(curated_transect_records))
  } else {
    return(dbplyr::remote_query(curated_transect_records))
  }
}

#' @rdname records_curated_view
#' @export
operation_curated_view <- function(con, return_data = FALSE) {

  raw_transects <- dplyr::tbl(con, dbplyr::in_schema("transect", "raw_transects"))

  curated_transects <- raw_transects %>%
    dplyr::group_by(transect_database_ID) %>%
    dplyr::filter(Timestamp == max(Timestamp, na.rm = TRUE))

  if(return_data) {
    return(dplyr::collect(curated_transects))
  } else {
    return(dbplyr::remote_query(curated_transects))
  }
}

#' @rdname records_curated_view
#' @export
project_curated_view <- function(con, return_data = FALSE) {

  raw_project_information <- dplyr::tbl(con, dbplyr::in_schema("transect", "raw_project_information"))

  curated_project_information <- raw_project_information %>%
    dplyr::group_by(transect_project_database_ID) %>%
    dplyr::filter(Timestamp == max(Timestamp, na.rm = TRUE))

  if(return_data) {
    return(dplyr::collect(curated_project_information))
  } else {
    return(dbplyr::remote_query(curated_project_information))
  }
}
