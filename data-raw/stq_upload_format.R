#### Step 1: Update weda and load packages ####
# I changed the package code so install the updated version
devtools::install_github("JustinCally/weda")
library(weda)
library(dplyr)

#### Step 2: Check the raw camtrap records are in your environment  ####
exists("raw_camtrap_records")
# If TRUE, good, if not load them in from the rds you saved (using readRDS function)

#### Step 3: Clean up the format of the records ####
# Remove Camera column as this is just a subfolder and has no use
# Make an empty SubStation Column
# Make an Iteration column (I'm guessing this is 1... unless there are older records)
raw_camtrap_records_cleaned <- raw_camtrap_records %>%
  dplyr::select(-Camera) %>%
  dplyr::mutate(Iteration = 1,
                SubStation = NA_character_)

#### Note you will need to add some more columns to theis data/rename some.
# Like metadata_species to metadata_Species
# Add metadata_Individuals (can be blank)
# Add metadata_Multiples (has to be filled in can just use 1)

#### Step 4: Get operational data + make and model ####
# If you want make and model need to have hard drive attached as it will revisit the first image for each site to get that info
stq_operation_aditional_info <- camtrap_operation_from_records(raw_camtrap_records_cleaned,
                                                               get_cam_model = TRUE)

#### Step 5: Write the Additional Operation Data to a csv (or other file) ####
write.csv(stq_operation_aditional_info, "stq_operation_aditional_info.csv")
