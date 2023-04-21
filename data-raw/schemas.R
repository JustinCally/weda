# source camtrap upload vignette
ksource <- function(x, ...) {
  library(knitr)
  source(purl(x, output = tempfile()), ...)
}
ksource("vignettes/camtrap-upload.Rmd")

camtrap_record_schema <- pointblank::col_schema(raw_camtrap_records_fixed)
# usethis::use_data(camtrap_record_schema, overwrite = TRUE)

camtrap_operation_schema <- pointblank::col_schema(operationdata %>%
                                                     dplyr::mutate(dplyr::across(dplyr::where(lubridate::is.difftime), ~ as.character(.))))
# usethis::use_data(camtrap_operation_schema, overwrite = TRUE)

camtrap_project_schema <- pointblank::col_schema(projectdata)
# usethis::use_data(camtrap_project_schema, overwrite = TRUE)
