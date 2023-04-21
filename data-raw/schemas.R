# source camtrap upload vignette
ksource <- function(x, ...) {
  library(knitr)
  source(purl(x, output = tempfile()), ...)
}
ksource("vignettes/camtrap-upload.Rmd")

camtrap_record_schema <- pointblank::col_schema(raw_camtrap_records_fixed)
usethis::use_data(camtrap_record_schema, internal = FALSE)

camtrap_operation_schema <- pointblank::col_schema(operationdata)
usethis::use_data(camtrap_operation_schema, internal = FALSE)

camtrap_project_schema <- pointblank::col_schema(projectdata)
usethis::use_data(camtrap_project_schema, internal = FALSE)
