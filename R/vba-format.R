#' VBA Upload format for camera trap data
#'
#' @description A presence-absence view of camera trap surveys using the VBA upload template
#'
#' @param con database connection
#' @param ProjectShortName ProjectShortName field
#' @param return_data logical flag to return data (TRUE) or sql query (default is FALSE)
#' @param schema which schema to use
#'
#' @return sql or data.frame
#' @export
#' @examples
#'  \dontrun{
#' con <- weda_connect(password = keyring::key_get(service = "ari-dev-weda-psql-01",
#' username = "psql_user"))
#' DBI::dbExecute(conn = con,
#'                paste(SQL("CREATE VIEW camtrap.processed_vba_format AS"),
#'                vba_format(con = con, return_data = FALSE)))
#' }
vba_format <- function(con,
                       ProjectShortName = "all",
                       return_data = FALSE,
                       schema = "camtrap") {

  if(ProjectShortName == "all") {
  presence_only <- dplyr::tbl(con, dbplyr::in_schema(schema, "processed_site_substation_presence_absence")) %>%
    dplyr::mutate(SiteStation = paste(SiteID, SubStation, sep = "_")) %>%
    dplyr::group_by(ProjectShortName, SiteStation, Iteration, common_name, scientific_name) %>%
    dplyr::summarise(Presence = max(Presence, na.rm = TRUE)) %>%
    dplyr::ungroup()
  } else {
    presence_only <- dplyr::tbl(con, dbplyr::in_schema(schema, "processed_site_substation_presence_absence")) %>%
      dplyr::filter(ProjectShortName %in% !!ProjectShortName) %>%
      dplyr::mutate(SiteStation = paste(SiteID, SubStation, sep = "_")) %>%
      dplyr::group_by(ProjectShortName, SiteStation, Iteration, common_name, scientific_name) %>%
      dplyr::summarise(Presence = max(Presence, na.rm = TRUE)) %>%
      dplyr::ungroup()
  }

  curated_camtrap_operation <- dplyr::tbl(con, dbplyr::in_schema(schema, "curated_camtrap_operation")) %>%
    dplyr::mutate(SiteStation = paste(SiteID, SubStation, sep = "_"))
  curated_project_information <- dplyr::tbl(con, dbplyr::in_schema(schema, "curated_project_information"))
  db_vba_name_conversions <- dplyr::tbl(con, dbplyr::in_schema(schema, "vba_name_conversions"))

  presence_cam_details <- presence_only %>%
    dplyr::left_join(curated_camtrap_operation, by = c("ProjectShortName", "SiteStation", "Iteration")) %>%
    dplyr::left_join(curated_project_information, by = c("ProjectShortName")) %>%
    dplyr::left_join(db_vba_name_conversions, by = c("scientific_name", "common_name")) %>%
    dplyr::transmute(`VBA Project ID` = NA,
                     `leave blank 1` = NA,
                     `leave blank 2` = NA,
                     `VBA Login name (comma separated)` = NA,
                     `VBA Site ID` = NA,
                     `Restricted Site (y)` = NA,
                     `leave blank 3` = NA,
                     `Site Name` = SiteStation,
                     `Location description` = ProjectDescription,
                     `Survey Name` = ProjectName,
                     `Start date (use this)` = DateDeploy,
                     `End date` = dplyr::coalesce(as.Date(Problem1_from), DateRetrieve),
                     `Type of coordinate system` = "decdegrees",
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
                     `Count` = Presence,
                     `Extra Information` = NA,
                       # paste("Presence = 1, Absence = 0 | The camera was:",
                       #                           BaitedUnbaited, "and was set at", CameraHeight,
                       #                           "m above the ground. The camera model was", CameraModel,
                       #                           "set to", CameraSensitivity, "sensitivity, with", CameraPhotosPerTrigger,
                       #                           "photos per trigger, and camera delay was", CameraDelay, ". The number of
                       #                           days the camera was deployed for is provided in the Sampling details field."),
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
