library(tidyverse)
library(sf)

# Samples of transect data
koala <- readRDS("~/Documents/Work/R Repositories/koala-vic/data/koala_recs_form.rds")

colnames(koala[[1]])

colnames(koala[[2]])

# Additional columns to add
records_base <- koala[[1]] %>%
  mutate(Iteration = 1L,
         SurveyMethod = "Diurnal double-observer distance-sampling",
         ColourForm = NA_character_,
         PhotoID = NA_character_,
         AnimalID = as.character(AnimalID),
         ObserverID = as.character(ObserverID),
         Transect = as.integer(Transect),
         Adults = as.integer(Adults),
         Joeys = as.integer(Joeys),
         Individuals = as.integer(Individuals),
         ObserverPosition = as.integer(ObserverPosition),
         DateTimeOriginal = case_when(SiteID == "OT10" ~ DateTimeOriginal + lubridate::hours(1),
                                      SiteID == "SWP5" & Transect == 2 & AnimalID == 10 ~ DateTimeOriginal - lubridate::hours(1),
                                      TRUE ~ DateTimeOriginal)) %>%
  select(-Date, -Time, -ObserverName, -ObserverID) %>%
  rename(DateTime = DateTimeOriginal,
         ObserverLongitude = Longitude,
         ObserverLatitude = Latitude) %>%
  weda::standardise_species_names(format = "common")

transects_base <- koala[[2]] %>%
  mutate(Iteration = 1L,
         TransectLength = st_length(geometry) %>% as.numeric(),
         MoonPhase = NA_character_,
         Cloud = NA,
         RelativeHumidity = NA,
         Wind = NA_character_,
         Precipitation = NA_character_,
         FlowerIndex = NA_character_,
         Access = NA_character_,
         TransectType = "Line",
         Transect = as.integer(Transect),
         ObserverPosition = as.integer(ObserverPosition),
         Visibility = case_when(AverageVisibility < 2 ~ "Poor",
                                AverageVisibility >= 2 & AverageVisibility < 4 ~ "Moderate",
                                AverageVisibility >= 4 ~ "Excellent"),
         MaxTruncationDistance = 100,
         ObserverID = as.character(AuthorID), ) %>%
  select(-Project, -FileID, -Heard, -Seen, -AverageVisibility, -AuthorID)

project_name_base <- data.frame('ProjectName' = "Statewide Koala Monitoring",
                                'ProjectShortName' = "statewide_koala",
                                'DistanceSampling' = TRUE,
                                'TerrestrialArboreal' = "Arboreal",
                                'AllSpeciesTagged' = FALSE,
                                'DistanceForAllSpecies' = FALSE,
                                'DiurnalNocturnal' = "Diurnal",
                                'ProjectDescription' = "Statewide monitoring of koalas using double-observer distance-sampling and acoustic monitoring",
                                'ProjectLeader' = "Justin Cally and Dave Ramsey")

visualise_records <- function(transects, records) {

  transects <- transects %>%
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

  vis_lines <- bind_rows(observer_records, animal_records) %>%
    dplyr::group_by(Iteration, SiteID, Transect, scientific_name, common_name, AnimalID) %>%
    dplyr::summarize(do_union=FALSE) %>%
    sf::st_cast("LINESTRING") %>%
    dplyr::ungroup()

  base_map <- mapview::mapview()

  transect_plot <- base_map@map %>%
    leaflet::addPolylines(data = transects, color = "#377eb8") %>%
    leaflet::addPolylines(data = animal_lines2, color = "orange") %>%
    leaflet::addCircles(data = animal_records2, color = "orange", radius = 2.5, fillOpacity = 0.8) %>%
    leaflet::addPolylines(data = vis_lines, color = "black") %>%
    leaflet::addCircles(data = animal_records, color = "#e41a1c", radius = 5) %>%
    leaflet::fitBounds(lng1 = bounds[1], lat1 = bounds[2], lng2 = bounds[3], lat2 = bounds[4])

  transect_plot
}
visualise_records(transects = transects_base, records = records_base)

# data quality
dq <- data_quality_transects(records = records_base,
                             transects = transects_base,
                             project_information = project_name_base)
