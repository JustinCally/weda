library(tidyverse)
library(sf)
library(weda)

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
  select(-Date, -Time, -ObserverName, -ObserverID, -LoR2, -SeenOnSameSide, -DistanceBetweenAnimalProj) %>%
  rename(DateTime = DateTimeOriginal,
         ObserverLongitude = Longitude,
         ObserverLatitude = Latitude) %>%
  weda::standardise_species_names(format = "common")

transects_base <- koala[[2]] %>%
  mutate(Iteration = 1L,
         TransectLength = st_length(geometry) %>% as.numeric(),
         MoonPhase = NA_integer_,
         Cloud = NA_integer_,
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


visualise_records(transects = transects_base, records = records_base)



# Check position of animals relative to transects
records_filterd <- filter_records_outside_transect_area(transects = transects_base,
                                                        records = records_base)

visualise_records(transects = transects_base, records = records_filterd)

# data quality
dq <- data_quality_transects(records = records_filterd,
                             transects = transects_base,
                             project_information = project_name_base)

transects_prepped <- prepare_transect_upload(dq)


dd1 <- data.frame(schema = "transects",
                 table_name = "raw_transect_records",
                 table_description = "Records of animals detected along transects",
                 table_type = "raw",
                 column_name = names(dq[["records"]][["tbl"]]),
                 column_class = c(sapply(dq[["records"]][["tbl"]],
                                         function(y) paste(class(y), collapse = ", "),
                                         simplify = TRUE)),
                 column_description = NA_character_,
                 accepted_values = NA_character_,
                 row.names = NULL,
                 mandatory = NA_character_)

dd2 <- data.frame(schema = "transects",
                  table_name = "raw_transect_details",
                  table_description = "Records of when and where transects were undertaken",
                  table_type = "raw",
                  column_name = names(dq[["transects"]][["tbl"]]),
                  column_class = c(sapply(dq[["transects"]][["tbl"]],
                                          function(y) paste(class(y), collapse = ", "),
                                          simplify = TRUE)),
                  column_description = NA_character_,
                  accepted_values = NA_character_,
                  row.names = NULL,
                  mandatory = NA_character_)

dd3 <- data.frame(schema = "transects",
                  table_name = "raw_project_information",
                  table_description = "Details of the project under which transects were searched",
                  table_type = "raw",
                  column_name = names(dq[["project_information"]][["tbl"]]),
                  column_class = c(sapply(dq[["project_information"]][["tbl"]],
                                          function(y) paste(class(y), collapse = ", "),
                                          simplify = TRUE)),
                  column_description = NA_character_,
                  accepted_values = NA_character_,
                  row.names = NULL,
                  mandatory = NA_character_)

dd <- bind_rows(list(dd1,dd2,dd3))

write.csv(dd, "data-raw/transect_data_dictionary_base.csv")
