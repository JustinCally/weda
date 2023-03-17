#' Assesses the data quality of camera trap records, operations and project information
#'
#' @param camtrap_records this is the dataframe that contains the camera trap records (recordTable from camtrapR)
#' @param camtrap_operation this is the dataframe that contains the information about the camera trap operation
#' @param project_information this is the dataframe that contains the information about the project
#'
#' @return list of pointblank objects
#' @export
camera_trap_dq <- function(camtrap_records,
                               camtrap_operation,
                               project_information) {

  req_cols <- c('SiteID' ,'SubStation', 'Iteration', 'scientific_name', 'common_name' ,'DateTimeOriginal' ,'Date' ,'Time' ,'delta.time.secs' ,'delta.time.mins' ,'delta.time.hours' ,'delta.time.days' ,'Directory' ,'FileName' ,'n_images' ,'HierarchicalSubject')
  # this is a vector of column names that are required to be in the camtrap_records dataframe


  # this is a vector of column names that are required to be in the camtrap_operation dataframe
  if(project_information$DistanceSampling) {

  # this is a vector of column names that are required to be in the project_information dataframe
    req_cols <- c(req_cols, "metadata_Distance")
  }
  # Create a pointblank object
  pb_rec <- pointblank::create_agent(
    tbl = camtrap_records,
    actions = pointblank::action_levels(stop_at = 1)) %>%
    pointblank::col_exists(columns = req_cols) %>%
    pointblank::rows_distinct() %>%
    pointblank::col_is_character(pointblank::vars(SiteID, SubStation, scientific_name, common_name, Time, Directory, FileName)) %>%
    pointblank::col_is_integer(pointblank::vars(Iteration)) %>%
    pointblank::col_vals_in_set(pointblank::vars(SiteID), set = camtrap_operation$SiteID) %>%
    pointblank::col_vals_in_set(pointblank::vars(SubStation), set = camtrap_operation$SubStation) %>%
    pointblank::col_vals_in_set(pointblank::vars(scientific_name), set = unique(vba_name_conversions$scientific_name)) %>%
    pointblank::col_vals_in_set(pointblank::vars(common_name), set = unique(vba_name_conversions$common_name)) %>%
    pointblank::col_vals_not_null(c("SiteID", "scientific_name", "common_name", "Date", "Time", "DateTimeOriginal", "Iteration")) %>%
    pointblank::col_is_date(pointblank::vars(Date)) %>%
    pointblank::col_is_posix(pointblank::vars(DateTimeOriginal)) %>%
    pointblank::interrogate()

  pb_op <- pointblank::create_agent(
    tbl = camtrap_operation,
    actions = pointblank::action_levels(stop_at = 1)) %>%
    pointblank::col_exists(columns = c('SiteID', 'SubStation', 'Iteration', 'Latitude', 'Longitude', 'DateDeploy', 'TimeDeploy', 'DateRetrieve', 'TimeRetrieve', 'Problem1_from', 'Problem1_to', 'DateTimeDeploy', 'DateTimeRetrieve', 'CameraHeight', 'CameraID')) %>%
    pointblank::rows_distinct() %>%
    pointblank::col_is_character(pointblank::vars(SiteID, SubStation, CameraID)) %>%
    pointblank::col_is_numeric(pointblank::vars(Latitude, Longitude, CameraHeight)) %>%
    pointblank::col_is_date(pointblank::vars(DateDeploy, DateRetrieve)) %>%
    pointblank::col_is_integer(pointblank::vars(Iteration)) %>%
    pointblank::col_is_posix(pointblank::vars(DateTimeDeploy, DateTimeRetrieve, Problem1_from, Problem1_to)) %>%
    pointblank::col_vals_in_set(pointblank::vars(SiteID), set = camtrap_records$SiteID) %>%
    pointblank::col_vals_in_set(pointblank::vars(SubStation), set = camtrap_records$SubStation) %>%
    pointblank::col_vals_between(columns = pointblank::vars(Latitude), left = -60.55, right = -8.47) %>%
    pointblank::col_vals_between(columns = pointblank::vars(Longitude), left = 93.41, right = 173.34) %>%
    pointblank::col_vals_not_null(c('SiteID', 'Latitude', 'Longitude', 'DateDeploy', 'TimeDeploy', 'DateRetrieve', 'TimeRetrieve', 'DateTimeDeploy', 'DateTimeRetrieve', 'CameraHeight', 'CameraID', 'Iteration')) %>%
    pointblank::interrogate()

  pb_pi <- pointblank::create_agent(
    tbl = project_information,
    actions = pointblank::action_levels(stop_at = 1)) %>%
    pointblank::row_count_match(1) %>%
    pointblank::col_vals_not_null(dplyr::everything()) %>%
    pointblank::col_is_logical(pointblank::vars(DistanceSampling, AllSpeciesTagged)) %>%
    pointblank::col_vals_in_set(pointblank::vars(TerrestrialArboreal), set = c("Terrestrial", "Arboreal")) %>%
    pointblank::col_vals_in_set(pointblank::vars(BaitedUnbaited), set = c("Baited", "Unbaited")) %>%
    pointblank::col_vals_in_set(pointblank::vars(BaitType), set = c("None", "Creamed Honey", "Small Mammal Bait", "Predator Bait")) %>%
    pointblank::interrogate()

  return(list(camtrap_records = pb_rec,
              camtrap_operation = pb_op,
              project_information = pb_pi))
}

