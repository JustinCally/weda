#' View the camera trap
#'
#' @param camtrap_operation_table camera trap operation table as found in the camtrap schema
#'
#' @return mapview
#' @export
camtrap_operation_mapview <- function(camtrap_operation_table) {

  sf_obj <- sf::st_as_sf(camtrap_operation_table, coords = c("Longitude", "Latitude"), crs = 4283)

  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(data = sf_obj %>% dplyr::mutate(SiteSubstation = paste(SiteID, SubStation, sep = " | ")), popup = ~SiteSubstation, opacity = 1, color = "black",
                              fillColor = "DarkRed", fillOpacity = 0.9, radius = 5, weight = 1)
}
