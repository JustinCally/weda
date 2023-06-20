## code to prepare `vba_name_conversions` dataset goes here
library(dplyr)
## Read in the DAT file
## Sourced from data.vic
con <- weda::weda_connect(password = keyring::key_get(service = "ari-dev-weda-psql-01", username = "psql_user"))
vba_name_conversions <- sf::st_read("data-raw/VBA_TAXA_LIST") %>%
  dplyr::select(taxon_id = TAXON_ID,
                scientific_name = SCI_NAME,
                common_name = COMM_NAME) %>%
  dplyr::distinct() %>%
  dplyr::filter(!(scientific_name %in% c("Vulpes spp.",
                                         "fam. Canidae gen. Vulpes",
                                         "fam. Cervidae gen. Dama",
                                         "fam. Corvidae gen. Corvus",
                                         "fam. Dasyuridae gen. Antechinus")))
# Remove dodgy conversions

# vba_name_conversions <- readRDS("data-raw/vba_name_conversions.rds")

usethis::use_data(vba_name_conversions, overwrite = TRUE)

DBI::dbWriteTable(con, DBI::Id(schema = "camtrap", table = "vba_name_conversions"),
                  vba_name_conversions, row.names = FALSE, append = FALSE, overwrite = TRUE)
