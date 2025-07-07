#' View the camera trap
#'
#' @param camtrap_operation_table camera trap operation table as found in the camtrap schema
#' @param records camera trap records table
#' @param species_name dynamic species name
#'
#'
#' @return mapview
#' @export

camtrap_operation_mapview <- function(camtrap_operation_table, records, species_name) {
  # Ensure required packages are attached in the NAMESPACE file of your package
  # Or use @importFrom directives in your roxygen2 documentation

  # Standardize SubStation and create SiteSubstation key
  camtrap_operation_table <- camtrap_operation_table %>%
    dplyr::mutate(
      SubStation = ifelse(is.na(SubStation), "", SubStation),
      SiteSubstation = paste(SiteID, SubStation, sep = " | ")
    )

  records <- records %>%
    dplyr::mutate(
      SubStation = ifelse(is.na(SubStation), "", SubStation),
      SiteSubstation = paste(SiteID, SubStation, sep = " | ")
    )

  # Create presence/absence table
  species_presence <- records %>%
    dplyr::filter(common_name == species_name) %>%
    dplyr::distinct(SiteSubstation) %>%
    dplyr::mutate(present = TRUE)

  # Join presence with operation data
  data_joined <- camtrap_operation_table %>%
    dplyr::left_join(species_presence, by = "SiteSubstation") %>%
    dplyr::mutate(present = ifelse(is.na(present), FALSE, present))

  # Convert to sf object
  sf_obj <- sf::st_as_sf(data_joined, coords = c("Longitude", "Latitude"), crs = 4283)

  # Define color palette for presence/absence
  pal <- leaflet::colorFactor(palette = c("darkred", "green"), domain = c(FALSE, TRUE))

  # Generate leaflet map
  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(
      data = sf_obj,
      popup = ~paste(SiteSubstation, "<br>", species_name, "present:", present),
      opacity = 1,
      color = "black",
      fillColor = ~pal(present),
      fillOpacity = 0.9,
      radius = 5,
      weight = 1
    ) %>%
    leaflet::addLegend(
      position = "bottomright",
      pal = pal,
      values = sf_obj$present,
      title = paste(species_name, "presence"),
      opacity = 1
    )
}
