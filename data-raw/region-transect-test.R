# Test DQ of Port Philip Data
library(tidyverse)
library(readxl)
data_dir <- "~/Documents/Work/R Repositories/blog/_posts/2024-07-18-database-gg/data"

# proofsafe_data <- read_csv(paste0(data_dir, '/Proofsafe Animals 2023/20231208050326_RezFile.csv'))
transect_data <- read_excel(paste0(data_dir, "/PortPhillip_GreaterGlider_2023.xlsx"), sheet = "SurveyDetails") %>%
  filter(!str_detect(Comments, "ABANDONED"))

transect <- transect_data

obs_data <- read_excel(paste0(data_dir, "/PortPhillip_GreaterGlider_2023.xlsx"), sheet = "ObsAttributes")

format_transect_regions <- function(transect, observations) {

  transects_long <- transect %>%
    tidyr::pivot_longer(cols = c("NameObs1", "NameObs2"), values_to = "ObserverName") %>%
    dplyr::mutate(ObserverPosition = case_when(name == "NameObs1" ~ 1L,
                                               name == "NameObs2" ~ 2L))

  transect_formatted <- transects_long %>%
    transmute(SiteID = SiteNumber,
              Transect = coalesce(TransectID, 1L),
              Iteration = SiteVisit,
              ObserverPosition,
              ObserverName,
              Date = DateSurvey,
              StartTime = as.POSIXct(paste(DateSurvey,
                                           format(TimeStart, "%H:%M:%OS"))),
              EndTime = as.POSIXct(paste(DateSurvey,
                                         format(TimeEnd, "%H:%M:%OS"))),
              Duration = EndTime - StartTime,
              Weather = NA_character_,
              Temperature = TempAir,
              TransectNotes = Comments,
              MoonPhase,
              Cloud,
              RelativeHumidity = RH,
              Wind,
              Precipitation,
              FlowerIndex,
              Access,
              Visibility = VisibilityHorizontal,
              TransectLength = `Spotlight Transect length`,
              MaxTruncationDistance = 100,
              EastingStart, EastingEnd, NorthingStart, NorthingEnd,
              TransectType = "Line")

  site_starts <- transect_formatted %>%
    sf::st_as_sf(coords = c("EastingStart", "NorthingStart"), crs = 28355)

  site_ends <- transect_formatted %>%
    sf::st_as_sf(coords = c("EastingEnd", "NorthingEnd"), crs = 28355)

  transect_line <- bind_rows(site_starts, site_ends) %>%
    dplyr::group_by(SiteID, Transect, Iteration) %>%
    dplyr::summarise() %>%
    sf::st_cast("LINESTRING") %>%
    sf::st_transform(4283) %>%
    dplyr::ungroup()

  transect_formatted_sf <- dplyr::left_join(transect_formatted %>%
                                       dplyr::select(-EastingStart, -EastingEnd,
                                                     -NorthingStart, -NorthingEnd),
                                     transect_line, by = join_by(SiteID, Transect, Iteration)) %>%
    sf::st_as_sf()

  records <- observations %>%
    dplyr::rename(SiteID = SiteNumber,
           Iteration = VisitNumber,
           ObserverName = NameObs)

  obs_pos <- fuzzyjoin::stringdist_left_join(records, transect_formatted %>%
    dplyr::select(SiteID, Transect, Iteration, ObserverName, ObserverPosition),
    by = c("SiteID", "Iteration", "ObserverName")) %>%
    dplyr::transmute(SiteID = SiteID.x,
                     Transect,
                     Iteration = Iteration.x,
                     common_name,
                     scientific_name,
                     DateTime = paste(DateObs, TimeSurvey),
                     SeenHeard = ObsType,
                     Adults = Count,
                     Joeys = NA_integer_,
                     Individuals = Count,
                     LoR = NA_character_,
                     WaypointNo = Waypoint,
                     EastingObserver,
                     NorthingObserver,
                     AnimalDistance = NA,
                     AnimalHeight = NA,
                     AnimalHorizontalDistance = Distance,
                     AnimalAnge = NA,
                     AnimalBearing = Bearing,
                     DistanceFromTransectStart = DistanceFromStart,
                     TreeSpecies = TreeType,
                     BothSeen = SeenByBoth,
                     ObservationNotes = Comments,
                     ObserverPosition,
                     SurveyMethod = "Nocturnal double-observer distance-sampling",
                     ColourForm = GGColourForm,
                     PhotoID) %>%
    st_as_sf(coords = c("EastingObserver", "NorthingObserver"), crs = 28355) %>%
    st_transform(4283) %>%
    mutate(Longitude = unlist(map(.$geometry,1)),
           Latitude = unlist(map(.$geometry,2))) %>%
    st_drop_geometry()

  records_grouped <- obs_pos %>%
    dplyr::distinct() %>%
    dplyr::group_by(SiteID, Transect, Iteration, scientific_name, WaypointNo) %>%
    dplyr::arrange(ObserverPosition) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  records_second <- obs_pos %>%
    dplyr::distinct() %>%
    dplyr::group_by(SiteID, Transect, Iteration, scientific_name, WaypointNo) %>%
    dplyr::arrange(ObserverPosition) %>%
    dplyr::slice(2) %>%
    dplyr::ungroup()

  # get destination points of animal
  proj_bearings <- geosphere::destPoint(p = as.matrix(records_grouped[, c("Longitude", "Latitude")]),
                                        b = records_grouped[["AnimalBearing"]],
                                        d = records_grouped[["AnimalHorizontalDistance"]]) %>%
    as.data.frame() %>%
    `colnames<-`(c("AnimalLongitude", "AnimalLatitude"))

  records_grouped <- dplyr::bind_cols(records_grouped, proj_bearings)

  proj_bearings2 <- geosphere::destPoint(p = as.matrix(records_second[, c("Longitude", "Latitude")]),
                                         b = records_second[["AnimalBearing"]],
                                         d = records_second[["AnimalHorizontalDistance"]]) %>%
    as.data.frame() %>%
    `colnames<-`(c("AnimalLongitude2", "AnimalLatitude2"))

  records_grouped_2 <- dplyr::bind_cols(records_second, proj_bearings2) %>%
    dplyr::select(SiteID, Transect, Date, scientific_name, AnimalID, AnimalLongitude2, AnimalLatitude2, LoR2 = LoR)

  records_combined <- dplyr::left_join(records_grouped, records_grouped_2) %>%
    dplyr::mutate(SeenOnSameSide = LoR == LoR2,
                  BothSeen = case_when(BothSeen == "Yes" ~ TRUE,
                                       BothSeen == "No" ~ FALSE,
                                       is.na(BothSeen) ~ !is.na(AnimalLongitude2)))

  return(records_combined)
}

obs_data_form <- standardise_species_names(recordTable = obs_data %>%
                                             mutate(CommonName = case_when(CommonName == "feather-tailed glider species"  ~ "Feather-tailed glider species",
                                                                           TRUE ~ CommonName)),
                                           format = "common", speciesCol = "CommonName", return_data = T)

formatted_transects <- format_transect_regions(transect = transect, observations = obs_data_form)

# Issues:
# Observer position not known for observation dataset
