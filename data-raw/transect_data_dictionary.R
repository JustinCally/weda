## Appends transect schema entries to the data_dictionary package object.
## Run this script whenever the transect data dictionary base CSV is updated.

library(dplyr)

# Load existing package data (camtrap schema only)
load("data/data_dictionary.rda")

# Read transect base (has accepted_values, mandatory instead of darwin_standard_core, derived_from)
transect_dd <- read.csv("data-raw/transect_data_dictionary_base.csv",
                        stringsAsFactors = FALSE) %>%
  select(-X) %>%
  mutate(schema = "transect") %>%          # normalise: "transects" -> "transect"
  select(schema, table_name, table_description, table_type,
         column_name, column_class, column_description) %>%
  mutate(darwin_standard_core = NA_character_,
         derived_from         = NA_character_)

data_dictionary <- bind_rows(
  data_dictionary,
  transect_dd
)

usethis::use_data(data_dictionary, overwrite = TRUE)
