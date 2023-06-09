#' Convert zone 54/55 easting/northings into lat/longs
#'
#' @param data data.frame with three columns (Easting, Northing, Zone)
#'
#' @return data but with Easting, Northing and Zone replaced with 'Longitude' and 'Latitude'
#' @export
convert_to_latlong <- function(data) {
  cols <- c("Easting", "Northing", "Zone")

  all_cols <- all(cols %in% colnames(data))

  if(all(c("Latitude", "Longitude") %in% colnames(data))) {

    mess <- "Data already has 'Latitude and Longitude" %>%
      `names<-`("v")

    cli::cli({
      cli::cli_bullets(c(mess))
    })

    return(data)

  } else if(!all_cols) {
    stop(paste0("Columns: ", cols, "are not present in the data"))
  } else {

  data_split <- split(data, data[["Zone"]])
  transformed_data <- list()

  for(i in 1:length(data_split)) {
    transformed_data[[i]] <- sf::st_as_sf(data_split[[i]],
                                          coords = c("Easting", "Northing"),
                                          crs = as.integer(paste0(283, unique(data_split[[i]][["Zone"]])))) %>%
      sf::st_transform(4283)
  }

  final_data <- dplyr::bind_rows(transformed_data) %>%
    dplyr::select(-"Zone") %>%
    cbind(dplyr::bind_rows(transformed_data) %>%
            sf::st_coordinates() %>%
            as.data.frame() %>%
            `colnames<-`(c("Longitude", "Latitude"))) %>%
    sf::st_drop_geometry()

  mess <- "Successfully converted zone 54/55 to Latitude and Longitude" %>%
    `names<-`("v")

  cli::cli({
    cli::cli_bullets(c(mess))
  })

  return(final_data)
  }

}

