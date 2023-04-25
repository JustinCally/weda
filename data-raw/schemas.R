# source camtrap upload vignette
ksource <- function(x, ...) {
  library(knitr)
  source(purl(x, output = tempfile()), ...)
}
ksource("vignettes/camtrap-upload.Rmd")

camtrap_record_schema <- pointblank::col_schema(raw_camtrap_records_fixed)
camera_trap_records <- raw_camtrap_records %>%
  mutate(metadata_Multiples = coalesce(as.integer(metadata_Multiples), 1L))


saveRDS(raw_camtrap_records, "inst/app/raw_camtrap_records.rds")
# usethis::use_data(camtrap_record_schema, overwrite = TRUE)

camtrap_operation_schema <- pointblank::col_schema(operationdata %>%
                                                     dplyr::mutate(dplyr::across(dplyr::where(lubridate::is.difftime), ~ as.character(.))))
# usethis::use_data(camtrap_operation_schema, overwrite = TRUE)

camtrap_project_schema <- pointblank::col_schema(projectdata)
# usethis::use_data(camtrap_project_schema, overwrite = TRUE)

library(openxlsx)
options("openxlsx.datetimeFormat" = "yyyy-mm-dd hh:mm:ss")
dd_filtered <- weda::data_dictionary %>%
  filter(table_type == "raw" & column_name %in% c(colnames(camera_trap_records),
                                                  colnames(operationdata),
                                                  colnames(projectdata)))

list_of_datasets <- list("camera_trap_records" = camera_trap_records,
                         "camera_trap_operation" = operationdata,
                         "camera_trap_project" = projectdata,
                         "data_dictionary" = dd_filtered)

write.xlsx(list_of_datasets, file = "inst/app/camera_trap_templates.xlsx", overwrite = TRUE)

