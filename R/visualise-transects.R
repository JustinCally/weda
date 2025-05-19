#' Visualise transects, records and sight-lines
#' @description View an interactive map of the transects and records as well as the lines from where the observer sighted them and the distance between projected animal locations
#'
#' @param records data.frame of the records with AnimalLongitude and AnimalLatitude columns
#' @param transects sf of the transects searched
#' @param endcap style of the endcap buffering, see \link[sf]{st_buffer}, default is 'FLAT' meaning the searchable area does not extend past the end of the transect
#'
#' @return mapview widget
#' @export
visualise_records <- function(records, transects, endcap = "FLAT") {

  transects_line <- transects %>%
    sf::st_transform(4283)

  transects <- transects %>%
    sf::st_transform(3111) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(geometry = sf::st_buffer(x = geometry,
                                dist = MaxTruncationDistance,
                                endCapStyle = endcap)) %>%
    sf::st_transform(4283)

  bounds <- sf::st_bbox(transects) %>%
    as.character()

  animal_records <- records %>%
    sf::st_as_sf(coords = c("AnimalLongitude", "AnimalLatitude"), crs = 4283)

  observer_records <- records %>%
    sf::st_as_sf(coords = c("ObserverLongitude", "ObserverLatitude"), crs = 4283)

  animal_records2 <- records %>%
    dplyr::filter(!is.na(AnimalLongitude2)) %>%
    sf::st_as_sf(coords = c("AnimalLongitude2", "AnimalLatitude2"), crs = 4283)

  animal_lines2 <- records %>%
    dplyr::filter(!is.na(AnimalLongitude2)) %>%
    sf::st_as_sf(coords = c("AnimalLongitude", "AnimalLatitude"), crs = 4283) %>%
    dplyr::bind_rows(animal_records2) %>%
    dplyr::group_by(Iteration, SiteID, Transect, scientific_name, common_name, AnimalID) %>%
    dplyr::summarize(do_union=FALSE) %>%
    sf::st_cast("LINESTRING") %>%
    dplyr::ungroup()

  vis_lines <- dplyr::bind_rows(observer_records, animal_records) %>%
    dplyr::group_by(Iteration, SiteID, Transect, scientific_name, common_name, AnimalID) %>%
    dplyr::summarize(do_union=FALSE) %>%
    sf::st_cast("LINESTRING") %>%
    dplyr::ungroup()

  base_map <- mapview::mapview()

  transect_plot <- base_map@map %>%
    leaflet::addPolylines(data = transects_line, color = "#377eb8") %>%
    leaflet::addPolygons(data = transects, fillColor = "#377eb8", weight = 0, popup = ~paste("Site:",
                                                                                             SiteID,
                                                                                             "Transect:",
                                                                                             Transect)) %>%
    leaflet::addPolylines(data = animal_lines2, color = "orange") %>%
    leaflet::addCircles(data = animal_records2, color = "orange", radius = 2.5, fillOpacity = 0.8,
                        popup = ~paste("Site:",
                                       SiteID,
                                       "Transect:",
                                       Transect,
                                       "AnimalID:",
                                       AnimalID,
                                       "Observer Position:",
                                       ObserverPosition)) %>%
    leaflet::addPolylines(data = vis_lines, color = "black") %>%
    leaflet::addCircles(data = animal_records, color = "#e41a1c", radius = 5,
                        popup = ~paste("Site:",
                                       SiteID,
                                       "Transect:",
                                       Transect,
                                       "AnimalID:",
                                       AnimalID,
                                       "Observer Position:",
                                       ObserverPosition)) %>%
    leaflet::fitBounds(lng1 = bounds[1], lat1 = bounds[2], lng2 = bounds[3], lat2 = bounds[4])

  return(transect_plot)
}
