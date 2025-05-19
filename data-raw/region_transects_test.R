# ARI Transects upload
library(weda)
library(tidyverse)
library(sf)
library(mapview)

# load IPA sites
# bbrr_sites <- sf::st_read(file.choose()) %>%
#   st_transform(3111)
#
# bbrr_sites_lines <- bbrr_sites %>%
#   group_by(SiteID = site) %>%
#   summarise(do_union=FALSE) %>%
#   st_cast("LINESTRING") %>%
#   mutate(Transect = NA) %>%
#   ungroup() %>%
#   st_as_sf()

# sf::write_sf(bbrr_sites_lines, "inst/dummydata/transectdata/gg_transects.geojson")

# proofsafe_records <- read_csv(file.choose())

proofsafe_records <- readr::read_csv("data-raw/region-test/20241206013455_RezFile.csv") %>%
  dplyr::filter(SiteID_H1 == "Road 21 C5 N2" & Species_I3 != "Unknown") %>%
  dplyr::mutate(SiteID_H1 = "C5")

# write_csv(proofsafe_records, "inst/dummydata/transectdata/region_gg_records.csv")

sites_lines <- sf::read_sf("data-raw/region-test/GG_Sites_Merged") %>%
  dplyr::filter(Name_12 %in% c("C5-START", "C5-END")) %>%
  dplyr::mutate(Name_12 = "C5") %>%
  sf::st_zm() %>%
  dplyr::mutate(DateTime = as.POSIXct(DateTime_1)) %>%
    dplyr::group_by(SiteID = Name_12) %>%
    dplyr::arrange(DateTime_1) %>%
  dplyr::summarise(do_union=FALSE) %>%
    sf::st_cast("LINESTRING") %>%
  dplyr::mutate(Transect = 1) %>%
  dplyr::ungroup() %>%
    sf::st_as_sf()

# sf::write_sf(sites_lines, "inst/dummydata/transectdata/region_gg_transects.geojson")

project_name_base <- data.frame('ProjectName' = "GG Fire Monitoring",
                                'ProjectShortName' = "gg_fire",
                                'DistanceSampling' = TRUE,
                                'TerrestrialArboreal' = "Arboreal",
                                'AllSpeciesTagged' = TRUE,
                                'DistanceForAllSpecies' = FALSE,
                                'DiurnalNocturnal' = "Nocturnal",
                                'ProjectDescription' = "Some description goes here",
                                'ProjectLeader' = "First Last")

# readr::write_csv(project_name_base, "inst/dummydata/transectdata/region_gg_project.csv")

proofsafe_data <- region_gg_proofsafe_format(proofsafe = proofsafe_records,
                                      gps_transects = sites_lines,
                                      Iteration = 1L,
                                      SurveyMethod = "Spotlight double-observer distance-sampling",
                                      MaxTruncationDistance = 500)

proofsafe_data_sp <- standardise_species_names(recordTable = proofsafe_data$records,
                                               format = "common",
                                               speciesCol = "Species")

visualise_records(transects = proofsafe_data$transects, records = proofsafe_data_sp)

# data quality
dq <- transect_dq(records = proofsafe_data_sp,
                  transects = proofsafe_data$transects,
                  project_information = project_name_base)

dq[[1]]
