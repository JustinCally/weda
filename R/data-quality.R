#' Check column schemas
#' @noRd
#'
#' @return pointblank object
check_col_schemas <- function(camtrap_records,
                              camtrap_operation,
                              project_information) {

  pb_rec_schema <- pointblank::create_agent(
    tbl = camtrap_records,
    actions = pointblank::action_levels(stop_at = 1)) %>%
    pointblank::col_schema_match(schema = weda::camtrap_record_schema,
                                 complete = T, is_exact = F) %>%
    pointblank::interrogate()

  pb_op_schema <- pointblank::create_agent(
    tbl = camtrap_operation %>%
      dplyr::mutate(dplyr::across(dplyr::where(lubridate::is.difftime), ~ as.character(.))),
    actions = pointblank::action_levels(stop_at = 1)) %>%
    pointblank::col_schema_match(schema = weda::camtrap_operation_schema,
                                 complete = T, is_exact = F) %>%
    pointblank::interrogate()

  pb_proj_schema <- pointblank::create_agent(
    tbl = project_information,
    actions = pointblank::action_levels(stop_at = 1)) %>%
    pointblank::col_schema_match(schema = weda::camtrap_project_schema,
                                 complete = T, is_exact = F) %>%
    pointblank::interrogate()

  return(list(camtrap_records = pb_rec_schema,
              camtrap_operation = pb_op_schema,
              project_information = pb_proj_schema))

}

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

  # this is a vector of column names that are required to be in the camtrap_operation dataframe
  req_cols <- c('SiteID' ,
                'SubStation',
                'Iteration',
                'scientific_name',
                'common_name' ,
                'DateTimeOriginal' ,
                'Date' ,
                'Time' ,
                'delta.time.secs' ,
                'delta.time.mins' ,
                'delta.time.hours' ,
                'delta.time.days' ,
                'Directory' ,
                'FileName' ,
                'n_images' ,
                'HierarchicalSubject',
                'metadata_Multiples',
                'metadata_Distance',
                'metadata_Individuals',
                'metadata_Behaviour',
                'metadata_Species')

  req_cols_op <- c('SiteID',
                   'SubStation',
                   'Iteration',
                   'Latitude',
                   'Longitude',
                   'DateDeploy',
                   'TimeDeploy',
                   'DateRetrieve',
                   'TimeRetrieve',
                   'Problem1_from',
                   'Problem1_to',
                   'DateTimeDeploy',
                   'DateTimeRetrieve',
                   'CameraHeight',
                   'CameraID',
                   'CameraModel',
                   'CameraSensitivity',
                   'CameraPhotosPerTrigger',
                   'CameraDelay')

  req_cols_proj <- c('ProjectName',
                     'ProjectShortName',
                     'DistanceSampling',
                     'TerrestrialArboreal',
                     'AllSpeciesTagged',
                     'BaitedUnbaited',
                     'BaitType')

  c1 <- colnames(camtrap_records)
  c2 <- colnames(camtrap_operation)
  c3 <- colnames(project_information)

  c1_c <- setdiff(c1,req_cols)
  c1_c2 <- setdiff(req_cols, c1)

  c2_c <- setdiff(c2,req_cols_op)
  c2_c2 <- setdiff(req_cols_op, c2)

  c3_c <- setdiff(c3,req_cols_proj)
  c3_c2 <- setdiff(req_cols_proj, c3)

  difflist <- list(c1_c, c1_c2, c2_c, c2_c2, c3_c, c3_c2)

  diffs <- lapply(difflist, length)

  if(sum(unlist(diffs)) > 0) {
    cli::cli_abort(c("Problem with column schema",
                     "Please correct (add/remove/rename) the following columns:",
                     unlist(difflist) %>% `names<-`(rep("x", length(unlist(difflist)))),
                     "\n",
                     "See weda::data_dictionary and weda::camtrap_record_schema,
                    weda::camtrap_operation_schema, weda::camtrap_project_schema for more information"))
  }



  if(project_information$DistanceSampling) {

  # this is a vector of column names that are required to be in the project_information dataframe
    req_cols <- c(req_cols, "metadata_Distance")
  }

  # vba names
  vba_sci <- weda::vba_name_conversions %>%
    dplyr::filter(.data$scientific_name %in% !!camtrap_records$scientific_name)

  vba_com <- weda::vba_name_conversions %>%
    dplyr::filter(.data$common_name %in% !!camtrap_records$common_name)

  # Create a pointblank object

pb_rec <- pointblank::create_agent(
    tbl = camtrap_records,
    actions = pointblank::action_levels(stop_at = 1)) %>%
    pointblank::col_exists(columns = req_cols) %>%
    pointblank::rows_distinct() %>%
    pointblank::col_is_character(c("SiteID", "SubStation", "scientific_name", "common_name", "Time", "Directory", "FileName")) %>%
    pointblank::col_is_integer(c("Iteration", "metadata_Multiples"))  %>%
    pointblank::col_vals_in_set("SiteID", set = camtrap_operation$SiteID) %>%
    pointblank::col_vals_in_set("SubStation", set = camtrap_operation$SubStation) %>%
    pointblank::col_vals_in_set("scientific_name", set = unique(vba_sci$scientific_name)) %>%
    pointblank::col_vals_in_set("common_name", set = unique(vba_com$common_name)) %>%
    pointblank::col_vals_not_null(c("SiteID", "scientific_name", "common_name", "Date", "Time", "DateTimeOriginal", "Iteration", "metadata_Multiples")) %>%
    pointblank::col_is_date("Date") %>%
    pointblank::col_is_posix("DateTimeOriginal") %>%
    pointblank::col_vals_between(columns = "Date",
                                 left = pointblank::vars(DateDeploy),
                                 right = pointblank::vars(DateRetrieve),
                                 inclusive = c(TRUE, TRUE),
                                 preconditions = function(x, lj = camtrap_operation) {
                                   dplyr::left_join(x, lj %>%
                                                      dplyr::select(dplyr::all_of(c("SiteID", "SubStation", "DateDeploy", "DateRetrieve"))),
                                                    by = c("SiteID", "SubStation"))
                                   }) %>%
    pointblank::interrogate()

pb_op <- pointblank::create_agent(
    tbl = camtrap_operation,
    actions = pointblank::action_levels(stop_at = 1)) %>%
    pointblank::col_exists(columns = c('SiteID', 'SubStation', 'Iteration', 'Latitude', 'Longitude', 'DateDeploy', 'TimeDeploy', 'DateRetrieve', 'TimeRetrieve', 'Problem1_from', 'Problem1_to', 'DateTimeDeploy', 'DateTimeRetrieve', 'CameraHeight', 'CameraID', 'CameraModel',	'CameraSensitivity',	'CameraDelay',	'CameraPhotosPerTrigger')) %>%
    pointblank::rows_distinct() %>%
    pointblank::col_is_character(columns = c('SiteID', 'SubStation', 'CameraID', 'CameraModel',	'CameraSensitivity',	'CameraDelay')) %>%
    pointblank::col_is_numeric(columns = c('Latitude', 'Longitude', 'CameraHeight')) %>%
    pointblank::col_is_date(columns = c('DateDeploy', 'DateRetrieve')) %>%
    pointblank::col_is_integer(columns = c('Iteration', 'CameraPhotosPerTrigger')) %>%
    pointblank::col_is_posix(columns = c('DateTimeDeploy', 'DateTimeRetrieve', 'Problem1_from', 'Problem1_to')) %>%
    pointblank::col_vals_in_set(columns = c('SiteID'), set = camtrap_records$SiteID, actions = pointblank::action_levels(stop_at = 0.99, warn_at = 1)) %>%
    pointblank::col_vals_in_set(columns = c('SubStation'), set = camtrap_records$SubStation, actions = pointblank::action_levels(stop_at = 0.99, warn_at = 1)) %>%
    pointblank::col_vals_between(columns = c('Latitude'), left = -60.55, right = -8.47) %>%
    pointblank::col_vals_between(columns = c('Longitude'), left = 93.41, right = 173.34) %>%
    pointblank::col_vals_not_null(c('SiteID', 'Latitude', 'Longitude', 'DateDeploy', 'TimeDeploy', 'DateRetrieve', 'TimeRetrieve', 'DateTimeDeploy', 'DateTimeRetrieve', 'CameraHeight', 'CameraID', 'Iteration', 'CameraModel',	'CameraSensitivity',	'CameraDelay',	'CameraPhotosPerTrigger')) %>%
    pointblank::interrogate()

  pb_pi <- pointblank::create_agent(
    tbl = project_information,
    actions = pointblank::action_levels(stop_at = 1)) %>%
    pointblank::row_count_match(1) %>%
    pointblank::col_vals_not_null(dplyr::everything()) %>%
    pointblank::col_is_logical(c("DistanceSampling", "AllSpeciesTagged")) %>%
    pointblank::col_vals_in_set("TerrestrialArboreal", set = c("Terrestrial", "Arboreal")) %>%
    pointblank::col_vals_in_set("BaitedUnbaited", set = c("Baited", "Unbaited")) %>%
    pointblank::col_vals_in_set("BaitType", set = c("None", "Creamed Honey", "Small Mammal Bait", "Predator Bait")) %>%
    pointblank::interrogate()

  return(list(camtrap_records = pb_rec,
              camtrap_operation = pb_op,
              project_information = pb_pi))
}
