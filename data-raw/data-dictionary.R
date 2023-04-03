# libraries
library(dplyr)
library(dbplyr)
library(DBI)

# Make connection
con_odbc <- RPostgreSQL::dbConnect(odbc::odbc(),
                                   Driver = "PostgreSQL Driver",
                                   Server = "10.110.7.201",
                                   Database = "ari-dev-weda-psql-01",
                                   UID = "psql_user",
                                   PWD = keyring::key_get(service = "ari-dev-weda-psql-01",
                                                          username = "psql_user"),
                                   Port = 5432,
                                   sslmode = "require",
                                   maxvarcharsize = 0)

## Select schema
schema <- "camtrap"

## Get tables
tables <- dbGetQuery(con_odbc,
           paste0("SELECT table_name FROM information_schema.tables
                   WHERE table_schema='",schema,"'"))

data_dict <- sapply(tables[,1], function(x) {

  tab <- tbl(con_odbc, in_schema(schema = schema, table = x)) %>%
                head() %>%
                collect()

df <- data.frame(schema = schema,
             table_name = x,
             table_description = NA_character_,
             table_type = stringr::str_extract(x, "[^_]+"),
             column_name = names(tab),
             column_class = c(sapply(tab,
                                     function(y) paste(class(y), collapse = ", "),
                                     simplify = TRUE)),
             column_description = NA_character_)

return(df)
}, simplify = F) %>% bind_rows() %>% `row.names<-`(NULL)

# cat(paste0("column_name == '", unique(data_dict$column_name), "' ~ ,", collapse = "\n"))

data_dictionary <- data_dict %>%
  mutate(table_description = case_when(table_name == "raw_camtrap_operation" ~ "Is a table that stores ALL camera trap deployments across projects (i.e. duplicates allowes). This is the table where uploads are appended to.",
                                       table_name == "raw_camtrap_records" ~ "Is a table that stores ALL camera trap records across projects (i.e. duplicate images allowed). This is the table where uploads are appended to.",
                                       table_name == "raw_project_information" ~ "Is a table that stores ALL project information entries (i.e. duplicate projects allowed). This is the table where uploads are appended to.",
                                       table_name == "curated_camtrap_operation" ~ "Is a table that stores the most recent entries for all camera trap deployments across projects (i.e. no duplicate sites)",
                                       table_name == "curated_camtrap_records" ~ "Is a table that stores the most recent camera trap records across projects (i.e. no duplicate images). This is based on a unique image being determined based on the project, site, substation, date-time and filename.",
                                       table_name == "curated_project_information" ~ "Is a table that stores the most recent project information entries (i.e. no duplicate projects)",
                                       table_name == "processed_site_substation_presence_absence" ~ "Is a table with the presence and absence of each species at each site. The possible absent species from each site are only derived from the species pool for a given project. This avoids absences of species for particular projects that did not set out to record that species.",
                                       table_name == "processed_site_substation_daily_presence_absence" ~ "Is a table where presences and absences are daily. Is a table with the presence and absence of each species at each site. The possible absent species from each site are only derived from the species pool for a given project. This avoids absences of species for particular projects that did not set out to record that species. Includes early truncation of camera deployment from the `curated_camtrap_operation` dataset when there is a problem with the operation period."),
         column_description = case_when(column_name == 'ProjectShortName' ~ "Short project name that data was collected for",
                                        column_name == 'SiteID' ~ "Site identification. Can be camera trap location or have a nesting of substations. Project specific.",
                                        column_name == 'SubStation' ~ "Nested location of camera trap within a site. Used in cases where a site has multiple camera trap deployments.",
                                        column_name == 'scientific_name' ~ "Scientific taxa name as per VBA",
                                        column_name == 'DateTimeOriginal' ~ "Date and time of photo-capture",
                                        column_name == 'Date' ~ "Date of photo",
                                        column_name == 'Time' ~ "Time of photo",
                                        column_name == 'delta.time.secs' ~ "Time lag between images (seconds)",
                                        column_name == 'delta.time.mins' ~ "Time lag between images (minutes)",
                                        column_name == 'delta.time.hours' ~ "Time lag between images (hours)",
                                        column_name == 'delta.time.days' ~ "Time lag between images (days)",
                                        column_name == 'Directory' ~ "Local directory path image was stored when metadata extracted",
                                        column_name == 'FileName' ~ "Filename of image when metadata extracted",
                                        column_name == 'metadata_Distance' ~ "Optional metadata field for distance bin (in metres)",
                                        column_name == 'metadata_Species' ~ "Metadata field for what species was used when tagging",
                                        column_name == 'n_images' ~ "Numer of images for record",
                                        column_name == 'metadata_Individuals' ~ "Metadata field for sex/age classification (e.g. Male1, Male2 for two males)",
                                        column_name == 'metadata_Behaviour' ~ "Optional metadata field for important behaviour (e.g. marker interaction)",
                                        column_name == 'metadata_Multiples' ~ "Mandatory metadata field for number of individuals in the photo",
                                        column_name == 'HierarchicalSubject' ~ "Full metadata string from the photo extracted by camtrapR",
                                        column_name == 'Iteration' ~ "Useful for multi-season surveys, this is the nth deployment iteration. If it is the first survey at a site, use 1.",
                                        column_name == 'common_name' ~ "Common taxa name as per VBA",
                                        column_name == 'camtrap_record_database_ID' ~ "Unique ID of the cameratrap record. ID is formulated from key variables of ProjectShortName, SiteID, SubStation, DateTimeOriginal, FileName",
                                        column_name == 'Timestamp' ~ "Time/date of upload (should exist as Greenwhich Mean Time)",
                                        column_name == 'Uploader' ~ "Name of person uploading data",
                                        column_name == 'geohash' ~ "8-digit geohash code based on longitude and latitude",
                                        column_name == 'Latitude' ~ "Latitude of camera in decimal degrees (EPSG: 4283)",
                                        column_name == 'Longitude' ~ "Longitude of camera in decimal degrees (EPSG: 4283)",
                                        column_name == 'DateDeploy' ~ "Date camera was deployed",
                                        column_name == 'TimeDeploy' ~ "Time camera was deployed",
                                        column_name == 'DateRetrieve' ~ "Date camera was retrieved",
                                        column_name == 'TimeRetrieve' ~ "Time camera was retrieved",
                                        column_name == 'Problem1_from' ~ "If there was a problem with the camera, when (date-time) did it start",
                                        column_name == 'Problem1_to' ~ "If there was a problem with the camera, when (date-time) did it end (usually when camera is picked up)",
                                        column_name == 'DateTimeDeploy' ~ "Date-time camera was deployed",
                                        column_name == 'DateTimeRetrieve' ~ "Date-time camera was deployed",
                                        column_name == 'CameraHeight' ~ "Height above ground of camera",
                                        column_name == 'CameraID' ~ "ID of camera (optional)",
                                        column_name == 'CameraModel' ~ "Model of camera",
                                        column_name == 'CameraSensitivity' ~ "Sensitivity of camera (low, medium, high or very high)",
                                        column_name == 'CameraDelay' ~ "Delay of camera in taking photos (Rapidfire or time in seconds)",
                                        column_name == 'CameraPhotosPerTrigger' ~ "Number of photos per trigger (e.g. 5)",
                                        column_name == 'camtrap_operation_database_ID' ~ "Unique ID of the cameratrap record. ID is formulated from key variables of ProjectShortName, SiteID, SubStation",
                                        column_name == 'ProjectName' ~ "Longer project name (as per official documents)",
                                        column_name == 'DistanceSampling' ~ "Logical flag (TRUE/FALSE), whether distance sampling was done for project",
                                        column_name == 'TerrestrialArboreal' ~ "Whether camera was Terrestrial or Arboreal",
                                        column_name == 'AllSpeciesTagged' ~ "Logical flag (TRUE/FALSE), whether all species seen were tagged",
                                        column_name == 'BaitedUnbaited' ~ "Whether camera was Baited or Unbaited",
                                        column_name == 'BaitType' ~ "Type of bait used: None, Creamed Honey, Small Mammal Bait or Predator Bait",
                                        column_name == 'camtrap_project_database_ID' ~ "Unique ID of the camera trap project ID is formulated from key variables of ProjectShortName"),
         darwin_standard_core = case_when(column_name == 'ProjectName' ~ NA_character_,
                                          column_name == 'ProjectShortName' ~ NA_character_,
                                          column_name == 'DistanceSampling' ~ NA_character_,
                                          column_name == 'TerrestrialArboreal' ~ NA_character_,
                                          column_name == 'AllSpeciesTagged' ~ NA_character_,
                                          column_name == 'BaitedUnbaited' ~ NA_character_,
                                          column_name == 'BaitType' ~ NA_character_,
                                          column_name == 'camtrap_project_database_ID' ~ NA_character_,
                                          column_name == 'Timestamp' ~ NA_character_,
                                          column_name == 'Uploader' ~ NA_character_,
                                          column_name == 'SiteID' ~ "locationID",
                                          column_name == 'SubStation' ~ "locationID",
                                          column_name == 'scientific_name' ~ "scientificName",
                                          column_name == 'DateTimeOriginal' ~ "verbatimEventDate",
                                          column_name == 'Date' ~ "eventDate",
                                          column_name == 'Time' ~ "eventTime",
                                          column_name == 'delta.time.secs' ~ NA_character_,
                                          column_name == 'delta.time.mins' ~ NA_character_,
                                          column_name == 'delta.time.hours' ~ NA_character_,
                                          column_name == 'delta.time.days' ~ NA_character_,
                                          column_name == 'Directory' ~ NA_character_,
                                          column_name == 'FileName' ~ NA_character_,
                                          column_name == 'metadata_Distance' ~ NA_character_,
                                          column_name == 'metadata_Species' ~ NA_character_,
                                          column_name == 'n_images' ~ NA_character_,
                                          column_name == 'metadata_Individuals' ~ "sex",
                                          column_name == 'metadata_Behaviour' ~ "behavior",
                                          column_name == 'metadata_Multiples' ~ 'individualCount',
                                          column_name == 'HierarchicalSubject' ~ 'occurrenceRemarks',
                                          column_name == 'Iteration' ~ NA_character_,
                                          column_name == 'common_name' ~ 'vernacularName',
                                          column_name == 'camtrap_record_database_ID' ~ 'occurrenceID',
                                          column_name == 'geohash' ~ 'georeferenceRemarks',
                                          column_name == 'Latitude' ~ 'decimalLatitude',
                                          column_name == 'Longitude' ~ 'decimalLongitude',
                                          column_name == 'DateDeploy' ~ 'eventDate',
                                          column_name == 'TimeDeploy' ~ 'eventTime',
                                          column_name == 'DateRetrieve' ~ 'latestDateCollected',
                                          column_name == 'TimeRetrieve' ~ 'eventTime',
                                          column_name == 'Problem1_from' ~ NA_character_,
                                          column_name == 'Problem1_to' ~ NA_character_,
                                          column_name == 'DateTimeDeploy' ~ NA_character_,
                                          column_name == 'DateTimeRetrieve' ~ NA_character_,
                                          column_name == 'CameraHeight' ~ NA_character_,
                                          column_name == 'CameraID' ~ NA_character_,
                                          column_name == 'CameraModel' ~ NA_character_,
                                          column_name == 'CameraSensitivity' ~ NA_character_,
                                          column_name == 'CameraDelay' ~ NA_character_,
                                          column_name == 'CameraPhotosPerTrigger' ~ NA_character_,
                                          column_name == 'camtrap_operation_database_ID' ~ NA_character_,
                                          column_name == 'Presence' ~ 'occurrenceStatus'),
         derived_from = case_when(column_name %in% c('TimeDeploy', 'DateRetrieve', 'TimeRetrieve', 'Problem1_from', 'Problem1_to', 'DateTimeDeploy', 'DateTimeRetrieve', 'CameraHeight', 'CameraID', 'CameraModel', 'CameraSensitivity', 'CameraDelay', 'CameraPhotosPerTrigger', 'DateDeploy', 'Longitude', 'Latitude','Iteration', 'SiteID', 'SubStation', 'ProjectName','ProjectShortName', 'DistanceSampling', 'TerrestrialArboreal', 'AllSpeciesTagged', 'BaitedUnbaited', 'BaitType') ~ "UserInput",
                                  column_name %in% c('Presence', 'camtrap_operation_database_ID', 'geohash', 'camtrap_record_database_ID', 'camtrap_project_database_ID', 'Timestamp', 'Uploader') ~ "automated",
                                  column_name %in% c('DateTimeOriginal', 'Date', 'Time', 'delta.time.secs', 'delta.time.mins', 'delta.time.hours', 'delta.time.days', 'Directory', 'FileName', 'metadata_Distance', 'metadata_Species', 'n_images', 'metadata_Individuals', 'metadata_Behaviour', 'metadata_Multiples', 'HierarchicalSubject') ~ "camtrapR",
                                  column_name %in% c('scientific_name', 'common_name') ~ "standardise_species_names()"))

usethis::use_data(data_dictionary, overwrite = TRUE)

DBI::dbWriteTable(con_odbc, DBI::Id(schema = "data_dictionary", table = "data_dictionary"),
                  data_dictionary, row.names = FALSE, append = FALSE, overwrite = TRUE)
