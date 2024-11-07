# transect data quality
# function that reverses a string by words
reverse_words <- function(string) {
  # split string by blank spaces
  string_split = strsplit(as.character(string), split = " ")
  # how many split terms?
  string_length = length(string_split[[1]])
  # decide what to do
  if (string_length == 1) {
    # one word (do nothing)
    reversed_string = string_split[[1]]
  } else {
    # more than one word (collapse them)
    reversed_split = string_split[[1]][string_length:1]
    reversed_string = paste(reversed_split, collapse = " ")
  }
  # output
  return(reversed_string)
}

#' Format proofsafe data for database
#'
#' @description
#' These functions format the datasets from proofsafe alongside gps data of the transect lines for koala or greater
#' glider (gg) surveys
#'
#'
#' @param proofsafe data.frame of data directly from proofsafe
#' @param gps_transects line transects, formatted as an sf object with columns for SiteID and Transect
#' @param sp_filter species filter
#'
#' @return list of data.frame for records and sf for transects
#' @export
koala_proofsafe_format <- function(proofsafe, gps_transects, sp_filter = "Koala") {

  records <- proofsafe %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Species_I3 = dplyr::na_if(Species_I3, "Other"),
           TreeSpecies_I3 = dplyr::na_if(TreeSpecies_I3, "Other"),
           Species = dplyr::coalesce(Species_I3, Animal_sp_other_I3)) %>%
    dplyr::filter(!is.na(Data_Section_Id_1161_I3) & SeenHeard_I3 == "Seen" & Species == sp_filter) %>% # has records
    dplyr::transmute(SiteID = SiteID_H1,
              Transect = Transect_H1,
              # Iteration = iteration,
              Species = Species,
              AnimalID = Animal_I3,
              Date = as.Date(lubridate::parse_date_time(Date_H1, c("ymd", "%m/%d/%Y"))),
              Time = AnimObsTime_I3,
              DateTimeOriginal = as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M:%OS"),
              SeenHeard = SeenHeard_I3,
              Adults = dplyr::coalesce(Adults_I3, 1),
              Joeys = dplyr::coalesce(Joeys_I3, 0),
              Individuals = Adults + Joeys,
              LoR = LorRtrans_I3,
              WaypointNo = `Waypoint _no_I3`,
              Latitude = min(Latitude_I3, Longitude_I3, na.rm = T), # fix for reversed input
              Longitude =  max(Latitude_I3, Longitude_I3, na.rm = T), # fix for reversed input
              AnimalDistance = DistToAnimal_I3,
              AnimalHeight = DistToAnimal_I3*sin(AngleToAnimal_I3*pi/180),
              AnimalHorizontalDistance = DistToAnimal_I3*cos(AngleToAnimal_I3*pi/180),
              AnimalAngle = AngleToAnimal_I3,
              AnimalBearing = BearingToAnimal_I3,
              DistanceFromTransectStart = Dist_F_Transect_I3,
              TreeSpecies = coalesce(TreeSpecies_I3, Tree_sp_other_I3),
              BothSeen = SeenX2_I3,
              ObservationNotes = Comments_I3,
              ObserverPosition = `Observer_H1...23`,
              ObserverName = Author_Name_F,
              ObserverID = Author_Id_F)

  records_grouped <- records %>%
    dplyr::distinct() %>%
    dplyr::group_by(SiteID, Transect, Date, Species, AnimalID) %>%
    dplyr::arrange(ObserverPosition) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  records_second <- records %>%
    dplyr::distinct() %>%
    dplyr::group_by(SiteID, Transect, Date, Species, AnimalID) %>%
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
    dplyr::select(SiteID, Transect, Date, Species, AnimalID, AnimalLongitude2, AnimalLatitude2, LoR2 = LoR)

  records_combined <- dplyr::left_join(records_grouped, records_grouped_2) %>%
    dplyr::mutate(SeenOnSameSide = LoR == LoR2,
                  BothSeen = case_when(BothSeen == "Yes" ~ TRUE,
                                       BothSeen == "No" ~ FALSE,
                                       is.na(BothSeen) ~ !is.na(AnimalLongitude2)))

  distance <- vector()

  for(i in 1:nrow(records_combined)) {
    if(!is.na(records_combined[i,"AnimalLongitude2"])) {
    distance[i] <- geosphere::distGeo(records_combined[i,c("AnimalLongitude", "AnimalLatitude")],
                           records_combined[i,c("AnimalLongitude2", "AnimalLatitude2")])
    } else {
      distance[i] <- NA
    }
  }

  records_combined$DistanceBetweenAnimalProj <- distance


  factor_vis <- function(x) {as.integer(factor(x, levels = c("0-25%", "26-50%", "51-75%", "76-100%")))}

  heard_or_seen <- proofsafe %>%
    dplyr::mutate(Species_I3 = dplyr::na_if(Species_I3, "Other"),
           Species = dplyr::coalesce(Species_I3, Animal_sp_other_I3)) %>%
    dplyr::filter(Species == sp_filter) %>% # has records
    dplyr::group_by(ProjectID_H1, SiteID_H1, Transect_H1, `Observer_H1...23`, Author_Id_F, Date_H1) %>%
    dplyr::summarise(Heard = any(SeenHeard_I3 == "Heard", na.rm = T),
              Seen = any(SeenHeard_I3 == "Seen", na.rm = T)) %>%
    dplyr::ungroup()

  transects <- proofsafe %>%
    dplyr::group_by(ProjectID_H1, SiteID_H1, Transect_H1, `Observer_H1...23`, Author_Id_F, Date_H1) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(heard_or_seen) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(AverageVisibility = mean(c(factor_vis(P1Vis_rank_H2),
                               factor_vis(P2Vis_rank_H2),
                               factor_vis(P3Vis_rank_H2),
                               factor_vis(P4Vis_rank_H2),
                               factor_vis(P5Vis_rank_H2),
                               factor_vis(P6Vis_rank_H2)), na.rm = TRUE),
           `Observer_H1...21` = na_if(`Observer_H1...21`, "Other"),
           Observer_c = dplyr::coalesce(`Observer_H1...21`, reverse_words(ObserverOther_H1))) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(SiteID = SiteID_H1,
              Transect = Transect_H1,
              FileID = File_Id_F,
              ObserverPosition = `Observer_H1...23`,
              ObserverName = Observer_c,
              AuthorID = Author_Id_F,
              Project = ProjectID_H1,
              Date = as.Date(parse_date_time(Date_H1, c("ymd", "%m/%d/%Y"))),
              StartTime = as.POSIXct(paste(Date, Start_time_H1), format = "%Y-%m-%d %H:%M:%OS"),
              EndTime = as.POSIXct(paste(Date, End_time_H2), format = "%Y-%m-%d %H:%M:%OS"),
              Duration = EndTime-StartTime,
              Weather = Weather_H1,
              Temperature = Temp_C_H1,
              TransectNotes = Notes_H2,
              AverageVisibility,
              Heard = dplyr::coalesce(Heard, FALSE),
              Seen = dplyr::coalesce(Seen, FALSE)) %>%
    dplyr::arrange(SiteID, Transect) %>%
    dplyr::left_join(gps_transects, by = c("SiteID", "Transect")) %>%
    sf::st_as_sf()

  #### Get perpindicular distance ####
  records_combined_sf <- records_combined %>%
    dplyr::select(SiteID, Transect, AnimalLongitude, AnimalLatitude) %>%
    sf::st_as_sf(coords = c("AnimalLongitude", "AnimalLatitude"), crs = 4283) %>%
    sf::st_transform(3111)

  transects_rec_order <- records_combined_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::left_join(transects %>%
                       dplyr::group_by(SiteID, Transect) %>%
                       dplyr::slice(1), by = c("SiteID", "Transect")) %>%
    sf::st_as_sf() %>%
    sf::st_transform(3111)

  records_combined$AnimalPerpDistance <- as.numeric(sf::st_distance(records_combined_sf,
                                            transects_rec_order,
                                            by_element = TRUE))

        return(list(records = records_combined,
                    transects = transects))
}

#' @rdname koala_proofsafe_format
#' @export
gg_proofsafe_format <- function(proofsafe, gps_transects) {

  records <- proofsafe %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Species_I3 = dplyr::na_if(Species_I3, "Other"),
                  `Tree species_I3` = dplyr::na_if(`Tree species_I3`, "Other"),
                  Species = dplyr::coalesce(Species_I3, Animal_sp_other_I3)) %>%
    dplyr::filter(!is.na(Data_Section_Id_340_I3)) %>% # has records
    dplyr::transmute(SiteID = SiteID_H1,
                     Transect = Transect_H1,
                     # Iteration = iteration,
                     Species = Species,
                     AnimalID = Animal_I3,
                     Date = as.Date(parse_date_time(Date_H1, c("ymd", "%d/%m/%Y", "%m/%d/%Y"))),
                     Time = AnimObsTime_I3,
                     DateTimeOriginal = as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M:%OS"),
                     SeenHeard = SeenHeard_I3,
                     Adults = 1,
                     Joeys = 0,
                     Individuals = 1,
                     LoR = `L or R of trans_I3`,
                     WaypointNo = `Waypoint no._I3`,
                     AnimalDistance = NA,
                     AnimalHeight = NA,
                     AnimalHorizontalDistance = dplyr::coalesce(`Distance to animal_I3`,0),
                     AnimalAngle = NA,
                     AnimalBearing = dplyr::coalesce(as.numeric(`Bearing to A._I3`), 0),
                     DistanceFromTransectStart = dplyr::coalesce(`Dist_F_Transect_I3`, 0),
                     TreeSpecies = coalesce(`Tree species_I3`, Tree_sp_other_I3),
                     BothSeen = SeenX2_I3,
                     ObservationNotes = Comments_I3,
                     ObserverPosition = `Observer_H1...20`,
                     ObserverName = dplyr::coalesce(na_if(`Observer_H1...18`, "Other"), ObserverOther_H1),
                     ObserverID = Author_Id_F)



  factor_vis <- function(x) {as.integer(factor(x, levels = c("0-25%", "26-50%", "51-75%", "76-100%")))}

  heard_or_seen <- proofsafe %>%
    dplyr::mutate(Species_I3 = dplyr::na_if(Species_I3, "Other"),
                  Species = dplyr::coalesce(Species_I3, Animal_sp_other_I3)) %>%
    dplyr::filter(!is.na(Data_Section_Id_340_I3)) %>% # has records
    dplyr::group_by(SiteID_H1, Transect_H1, `Observer_H1...20`, Author_Id_F, Date_H1) %>%
    dplyr::summarise(Heard = any(SeenHeard_I3 == "Heard", na.rm = T),
                     Seen = any(SeenHeard_I3 == "Seen", na.rm = T)) %>%
    dplyr::ungroup()

  transects <- proofsafe %>%
    dplyr::group_by(SiteID_H1, Transect_H1, `Observer_H1...20`, Author_Id_F, Date_H1) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(heard_or_seen) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(AverageVisibility = mean(c(factor_vis(`P1 Vis_rank_H2`),
                                             factor_vis(`P2 Vis_rank_H2`),
                                             factor_vis(`P3 Vis_rank_H2`),
                                             factor_vis(`P4 Vis_rank_H2`),
                                             factor_vis(`P5 Vis_rank_H2`),
                                             factor_vis(`P6 Vis_rank_H2`)), na.rm = TRUE),
                  `Observer_H1...18` = na_if(`Observer_H1...18`, "Other"),
                  Observer_c = dplyr::coalesce(`Observer_H1...18`, ObserverOther_H1)) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(SiteID = SiteID_H1,
                     Transect = Transect_H1,
                     FileID = File_Id_F,
                     ObserverPosition = `Observer_H1...20`,
                     ObserverName = Observer_c,
                     AuthorID = Author_Id_F,
                     Project = NA,
                     Date = as.Date(parse_date_time(Date_H1, c("ymd", "%d/%m/%Y", "%m/%d/%Y"))),
                     StartTime = as.POSIXct(paste(Date, Start_time_H1), format = "%Y-%m-%d %H:%M:%OS"),
                     EndTime = as.POSIXct(paste(Date, End_time_H2), format = "%Y-%m-%d %H:%M:%OS"),
                     Duration = EndTime-StartTime,
                     Weather = Weather_H1,
                     Temperature = Temp_C_H1,
                     TransectNotes = Notes_H2,
                     AverageVisibility,
                     Heard = dplyr::coalesce(Heard, FALSE),
                     Seen = dplyr::coalesce(Seen, FALSE)) %>%
    dplyr::arrange(SiteID, Transect) %>%
    dplyr::left_join(gps_transects, by = c("SiteID", "Transect")) %>%
    sf::st_as_sf() %>%
    sf::st_transform(4283)

  ## add in animal points ##

  records_locs <- records %>%
    dplyr::left_join(gps_transects, by = c("SiteID", "Transect")) %>%
    sf::st_as_sf() %>%
    sf::st_transform(3111) %>%
    dplyr::mutate(line_distance = sf::st_length(geometry)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(geometry = lwgeom::st_linesubstring(geometry,
                                               from = 0,
                                               to = DistanceFromTransectStart/line_distance) %>%
                                      lwgeom::st_endpoint() %>% st_sfc(crs = 3111)) %>%
    dplyr::ungroup() %>%
    sf::st_transform(4283) %>%
    dplyr::bind_cols(sf::st_coordinates(.) %>% `colnames<-`(c("Longitude", "Latitude"))) %>%
    sf::st_drop_geometry()

  # first and second obs
  records_grouped <- records_locs %>%
    dplyr::distinct() %>%
    dplyr::group_by(SiteID, Transect, Date, Species, AnimalID) %>%
    dplyr::arrange(ObserverPosition) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  records_second <- records_locs %>%
    dplyr::distinct() %>%
    dplyr::group_by(SiteID, Transect, Date, Species, AnimalID) %>%
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
    dplyr::select(SiteID, Transect, Date, Species, AnimalID, AnimalLongitude2, AnimalLatitude2, LoR2 = LoR)

  records_combined <- dplyr::left_join(records_grouped, records_grouped_2) %>%
    dplyr::mutate(SeenOnSameSide = LoR == LoR2,
                  BothSeen = case_when(BothSeen == "Yes" ~ TRUE,
                                       BothSeen == "No" ~ FALSE,
                                       is.na(BothSeen) ~ !is.na(AnimalLongitude2)))

  distance <- vector()

  for(i in 1:nrow(records_combined)) {
    if(!is.na(records_combined[i,"AnimalLongitude2"])) {
      distance[i] <- geosphere::distGeo(records_combined[i,c("AnimalLongitude", "AnimalLatitude")],
                                        records_combined[i,c("AnimalLongitude2", "AnimalLatitude2")])
    } else {
      distance[i] <- NA
    }
  }

  records_combined$DistanceBetweenAnimalProj <- distance



  #### Get perpindicular distance ####
  records_combined_sf <- records_combined %>%
    dplyr::select(SiteID, Transect, AnimalLongitude, AnimalLatitude) %>%
    sf::st_as_sf(coords = c("AnimalLongitude", "AnimalLatitude"), crs = 4283) %>%
    sf::st_transform(3111)

  transects_rec_order <- records_combined_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::left_join(transects %>%
                       dplyr::group_by(SiteID, Transect) %>%
                       dplyr::slice(1), by = c("SiteID", "Transect")) %>%
    sf::st_as_sf() %>%
    sf::st_transform(3111)

  records_combined$AnimalPerpDistance <- as.numeric(sf::st_distance(records_combined_sf,
                                                                    transects_rec_order,
                                                                    by_element = TRUE))

  return(list(records = records_combined,
              transects = transects))
}
