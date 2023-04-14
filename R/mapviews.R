#' View the camera trap
#'
#' @param camtrap_operation_table camera trap operation table as found in the camtrap schema
#'
#' @return mapview
#' @export
camtrap_operation_mapview <- function(camtrap_operation_table) {

  mapview::mapview(camtrap_operation_table,
                   xcol = "Longitude",
                   ycol = "Latitude",
                   crs = 4283)
}
