# Package index

## Database connection

Functions used to connect to the database

- [`weda_connect()`](https://justincally.github.io/weda/reference/weda_connect.md)
  : Database Connection

## Camera Trap Processing

Functions used to process, check and upload camera trap data

- [`standardise_species_names()`](https://justincally.github.io/weda/reference/standardise_species_names.md)
  : This function standardises species names in a dataframe
- [`vba_name_conversions`](https://justincally.github.io/weda/reference/vba_name_conversions.md)
  : Name conversions between scientific and common names
- [`camera_trap_dq()`](https://justincally.github.io/weda/reference/camera_trap_dq.md)
  : Camera Trap Data Quality Checks
- [`prepare_camtrap_upload()`](https://justincally.github.io/weda/reference/prepare_camtrap_upload.md)
  : Prepare camera trap data for upload to the database
- [`upload_camtrap_data()`](https://justincally.github.io/weda/reference/upload_camtrap_data.md)
  : Upload camera trap data to database
- [`camtrap_operation_from_records()`](https://justincally.github.io/weda/reference/camtrap_operation_from_records.md)
  : Operation from records
- [`camtrap_operation_mapview()`](https://justincally.github.io/weda/reference/camtrap_operation_mapview.md)
  : View the camera trap
- [`check_unique_project()`](https://justincally.github.io/weda/reference/check_unique_project.md)
  : check whether project is unique or already on database
- [`convert_to_latlong()`](https://justincally.github.io/weda/reference/convert_to_latlong.md)
  : Convert zone 54/55 easting/northings into lat/longs
- [`mcrecordTable()`](https://justincally.github.io/weda/reference/mcrecordTable.md)
  : Camera Trap Record Table for subset sites and using multiple cores
- [`proofsafe_to_operation()`](https://justincally.github.io/weda/reference/proofsafe_to_operation.md)
  : Proofsafe to operation

## Database views

Functions used to create views on the database

- [`transect_records_curated_view()`](https://justincally.github.io/weda/reference/records_curated_view.md)
  [`transect_curated_view()`](https://justincally.github.io/weda/reference/records_curated_view.md)
  [`transect_project_curated_view()`](https://justincally.github.io/weda/reference/records_curated_view.md)
  [`records_curated_view()`](https://justincally.github.io/weda/reference/records_curated_view.md)
  [`operation_curated_view()`](https://justincally.github.io/weda/reference/records_curated_view.md)
  [`project_curated_view()`](https://justincally.github.io/weda/reference/records_curated_view.md)
  : Curated views
- [`processed_SubStation_presence_absence()`](https://justincally.github.io/weda/reference/processed_SubStation_presence_absence.md)
  [`processed_transect_presence_absence()`](https://justincally.github.io/weda/reference/processed_SubStation_presence_absence.md)
  : Presence/Absence Views
- [`vba_format()`](https://justincally.github.io/weda/reference/vba_format.md)
  : VBA Upload format for camera trap data

## Database information

Functions used get information on the database

- [`data_dictionary`](https://justincally.github.io/weda/reference/data_dictionary.md)
  : Data Dictionary

## Shiny App Modules

Functions used in the shiny app

- [`projectMapUI()`](https://justincally.github.io/weda/reference/projectMapUI.md)
  [`projectMapServer()`](https://justincally.github.io/weda/reference/projectMapUI.md)
  : Shiny Map UI for Projects
- [`dataUploadpUI()`](https://justincally.github.io/weda/reference/dataUploadpUI.md)
  [`dataUploadServer()`](https://justincally.github.io/weda/reference/dataUploadpUI.md)
  : Data Upload Shiny Module
- [`camtrap_app()`](https://justincally.github.io/weda/reference/camtrap_app.md)
  [`transect_app()`](https://justincally.github.io/weda/reference/camtrap_app.md)
  : Run the camera trap app
- [`proofsafeDownloadUI()`](https://justincally.github.io/weda/reference/proofsafeDownloadUI.md)
  [`proofsafeDownloadServer()`](https://justincally.github.io/weda/reference/proofsafeDownloadUI.md)
  : proofsafe formatter module

## Obtaining Data

Functions used get data from the database

- [`DBdetectionHistory()`](https://justincally.github.io/weda/reference/DBdetectionHistory.md)
  : Database to detion histories

## Transect Upload Tools

- [`filter_records_outside_transect_area()`](https://justincally.github.io/weda/reference/filter_records_outside_transect_area.md)
  : Filter records outside transect area
- [`koala_proofsafe_format()`](https://justincally.github.io/weda/reference/koala_proofsafe_format.md)
  [`gg_proofsafe_format()`](https://justincally.github.io/weda/reference/koala_proofsafe_format.md)
  [`region_gg_proofsafe_format()`](https://justincally.github.io/weda/reference/koala_proofsafe_format.md)
  : Format proofsafe data for database
- [`prepare_transect_upload()`](https://justincally.github.io/weda/reference/prepare_transect_upload.md)
  : Prepare transect data for upload to the database
- [`transectMapUI()`](https://justincally.github.io/weda/reference/transectMapUI.md)
  [`transectMapServer()`](https://justincally.github.io/weda/reference/transectMapUI.md)
  : Shiny Map UI for Projects
- [`transect_dq()`](https://justincally.github.io/weda/reference/transect_dq.md)
  : Transect Data Quality Checks
- [`transectdataUploadpUI()`](https://justincally.github.io/weda/reference/transectdataUploadpUI.md)
  [`transectdataUploadServer()`](https://justincally.github.io/weda/reference/transectdataUploadpUI.md)
  : Data Upload Shiny Module
- [`upload_transect_data()`](https://justincally.github.io/weda/reference/upload_transect_data.md)
  : Upload camera trap data to database
- [`visualise_records()`](https://justincally.github.io/weda/reference/visualise_records.md)
  : Visualise transects, records and sight-lines
