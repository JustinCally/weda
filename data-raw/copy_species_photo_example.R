#### Database connection ####
library(magick)
con <- weda::weda_connect(password = keyring::key_get(service = "ari-dev-weda-psql-01",
                                                      username = "psql_user"), username = "psql_user")

dingo_photos <- dplyr::tbl(con, dbplyr::in_schema("camtrap", "curated_camtrap_records")) %>%
  dplyr::filter(ProjectShortName %in% "hog_deer_2023" & common_name == "Dingo") %>%
  dplyr::collect()

dingo_photos_fn <- paste(dingo_photos$Directory, dingo_photos$FileName, sep = "/")

file.copy(from=dingo_photos_fn, to="/Volumes/Cally_Camtr/DingoPhotos2",
          overwrite = TRUE, recursive = FALSE,
          copy.mode = TRUE)
