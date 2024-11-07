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

proofsafe_records_bbrr <- read_csv("inst/dummydata/transectdata/gg_proofsafe.csv")

bbrr_sites_lines <- sf::read_sf("inst/dummydata/transectdata/gg_transects.geojson")

project_name_base <- data.frame('ProjectName' = "BBRR Monitoring",
                                'ProjectShortName' = "bbrr",
                                'DistanceSampling' = TRUE,
                                'TerrestrialArboreal' = "Arboreal",
                                'AllSpeciesTagged' = TRUE,
                                'DistanceForAllSpecies' = FALSE,
                                'DiurnalNocturnal' = "Diurnal",
                                'ProjectDescription' = "BBRR",
                                'ProjectLeader' = "Jemma Cripps")

proofsafe_data <- gg_proofsafe_format(proofsafe = proofsafe_records_bbrr,
                                      gps_transects = bbrr_sites_lines,
                                      Iteration = 1L,
                                      SurveyMethod = "Spotlight double-observer distance-sampling",
                                      MaxTruncationDistance = 500)

proofsafe_data_sp <- standardise_species_names(recordTable = proofsafe_data$records %>%
                                                 mutate(Species = case_when(Species == "Deer - sambar?" ~ "Sambar Deer",
                                                                            Species %in% c("G vic",
                                                                                           "Geocrinia victoriana") ~ "Victorian Smooth Froglet",
                                                                            Species == "White striped freetail" ~ "White-striped Free-tailed Bat",
                                                                            Species == "Feather-tailed Glider" ~ "Feather-tailed glider species",
                                                                            Species == "Swamp wallaby" ~ "Black-tailed Wallaby",
                                                                            Species == "Tyto sp." ~ "Unidentified Tyto",
                                                                            TRUE ~ Species)) %>%
                                                 filter(!is.na(Species)),
                                               format = "common",
                                               speciesCol = "Species")

visualise_records(transects = proofsafe_data$transects, records = proofsafe_data_sp)

# data quality
dq <- transect_dq(records = proofsafe_data_sp,
                  transects = proofsafe_data$transects,
                  project_information = project_name_base)

dq[[1]]
