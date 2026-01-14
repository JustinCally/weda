# Changelog

## weda 0.0.13

- Allow for sites/projects to be hidden by writing a shiny_hide_table to
  the database

## weda 0.0.12

- Change the way the upload process works so that only recent data is
  refreshed

## weda 0.0.11

- Add in ability to convert proofsafe output to operations table

## weda 0.0.10

- Add ability to view presence absence across sites in upload

## weda 0.0.9

- Now connects to migrated database: ari-weda-flexi-psql-01

## weda 0.0.8

- Changed connection string for database

## weda 0.0.7

- Bait type now allows for: None, Creamed Honey, Small Mammal Bait,
  Predator Bait (i.e, meat bait), Non-toxic curiosity bait, Toxic
  curiosity bait, Predator Lure (i.e., urine, faeces, etc.) or Other.
- When plotting data in upload SiteID and SubStation are both shown in
  popup

## weda 0.0.6

- Refresh materialized view with code (not triggers) as it was not
  working. Refreshes on upload now

## weda 0.0.5

- VBA output format has been amended.

## weda 0.0.4

- Have a `DBDetectionHistory()` to get camtrap data in observation
  periods (e.g. for use in unmarked)  
- Additional vignette on how to download and use data  
- Create spatial view for camera trap deployments  
- Fixes to data quality to check unique iteration, siteID and substation
  combinations

## weda 0.0.3

- Use
  [`RPostgres::Postgres()`](https://rpostgres.r-dbi.org/reference/Postgres.html)
  as default connection method  
- `camtrap_operation_from_records` helps make operation table for date
  times and camera model
- VBA data format view and download available through the app  
- Additional options available for bait type during upload  
- [`mcrecordTable()`](https://justincally.github.io/weda/reference/mcrecordTable.md)
  allows a more customised way of extracting camera trap data

## weda 0.0.2

- Fixed issue with multiple iterations not joining in data quality
- Added help boxes to shiny app  
- shinyBS loads properly
- dataquality returns messages in app

## weda 0.0.1

- First main iteration of the app
- Vignette and tools to help upload camera trap data  
- Tools to help connect to database  
- Shiny pp to view, download and upload camera trap data to the
  database  
- Code for database views  
- Functions to standardize names to VBA syntax  
- Data dictionaries and schema for the camera trap database tables
  data(‘data_dictionary’)

## weda 0.0.0.9000

- Created dev package
