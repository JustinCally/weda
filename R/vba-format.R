vba_format <- function(con,
                       return_data = FALSE) {

  presence_only <- dplyr::tbl(con, dbplyr::in_schema("camtrap", "curated_camtrap_records")) %>%
    dplyr::group_by(ProjectShortName, SiteID, SubStation, Iteration, common_name, scientific_name) %>%
    dplyr::summarise(Date = max(Date, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Count = 1)

  curated_camtrap_operation <- dplyr::tbl(con, dbplyr::in_schema("camtrap", "curated_camtrap_operation"))
  curated_project_information <- dplyr::tbl(con, dbplyr::in_schema("camtrap", "curated_project_information"))
  db_vba_name_conversions <- dplyr::tbl(con, dbplyr::in_schema("camtrap", "vba_name_conversions"))

  presence_cam_details <- presence_only %>%
    dplyr::left_join(curated_camtrap_operation, by = c("ProjectShortName", "SiteID", "SubStation", "Iteration")) %>%
    dplyr::left_join(curated_project_information, by = c("ProjectShortName")) %>%
    dplyr::left_join(db_vba_name_conversions, by = c("scientific_name", "common_name")) %>%
    dplyr::transmute(`VBA Project ID` = NA,
                  `leave blank 1` = NA,
                  `leave blank 2` = NA,
                  `VBA Login name (comma separated)` = NA,
                  `VBA Site ID` = NA,
                  `Restricted Site (y)` = NA,
                  `leave blank 3` = NA,
                  `Site Name` = paste(SiteID, SubStation, sep = "_"),
                  `Location description` = ProjectDescription,
                  `Survey Name` = ProjectName,
                  `Start date (use this)` = DateDeploy,
                  `End date` = dplyr::coalesce(as.Date(Problem1_from), DateRetrieve),
                  `Type of coordinate system` = "latlong",
                  `GPS used (y, n)` = "y",
                  `Datum code (g,a)` = "g",
                  `X-coordinate (easting or longitude)` = Longitude,
                  `Y-coordinate (northing or latitude)` = Latitude,
                  `Mapsheet number (1:100k)` = NA,
                  `MGA Zone` = NA,
                  `Positional accuracy (metres)` = 10,
                  `Altitude (m asl)` = NA,
                  `Users own reference` = NA,
                  `Method code` = 9090,
                  `Method details code` = 101,
                  `leave blank 4` = NA,
                  `Sampling details - Numeric Value` = round(DateRetrieve-DateDeploy),
                  `Sampling details - Text value` = NA,
                  `Taxon Name` = scientific_name,
                  `Taxon Common Name` = common_name,
                  `Taxon ID` = taxon_id,
                  `Count` = Count,
                  `Extra Information` = "Presence-only",
                  `Type of Observation` = "o",
                  `Behaviour of taxa` = NA,
                  `Count Qualifier (e.g. 1-M/2-AF)` = NA,
                  `Origin of Taxon at site` = NA,
                  `Specimen sent to` = NA,
                  `Specimen reference No.` = NA,
                  `Observer One of Contributors Login Name` = NA
  )

  if(return_data) {
    return(dplyr::collect(presence_cam_details))
  } else {
    return(dbplyr::remote_query(presence_cam_details))
  }
}
