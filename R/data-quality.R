# check_col_schemas <- function(camtrap_records,
#                               camtrap_operation,
#                               project_information) {
#
#   pb_rec_schema <- pointblank::create_agent(
#     tbl = camtrap_records,
#     actions = pointblank::action_levels(stop_at = 1)) %>%
#     pointblank::col_schema_match(schema = weda::camtrap_record_schema,
#                                  complete = T, is_exact = F) %>%
#     pointblank::interrogate()
#
#   pb_op_schema <- pointblank::create_agent(
#     tbl = camtrap_operation %>%
#       dplyr::mutate(dplyr::across(dplyr::where(lubridate::is.difftime), ~ as.character(.))),
#     actions = pointblank::action_levels(stop_at = 1)) %>%
#     pointblank::col_schema_match(schema = weda::camtrap_operation_schema,
#                                  complete = T, is_exact = F) %>%
#     pointblank::interrogate()
#
#   pb_proj_schema <- pointblank::create_agent(
#     tbl = project_information,
#     actions = pointblank::action_levels(stop_at = 1)) %>%
#     pointblank::col_schema_match(schema = weda::camtrap_project_schema,
#                                  complete = T, is_exact = F) %>%
#     pointblank::interrogate()
#
#   return(list(camtrap_records = pb_rec_schema,
#               camtrap_operation = pb_op_schema,
#               project_information = pb_proj_schema))
#
# }

#' Report column schema problems for each uploaded table
#'
#' @param tables named list (names are user-facing table labels) of lists with
#'   `data` and `required` column names
#'
#' @noRd
#'
#' @return TRUE if all tables have exactly the required columns
report_column_schema <- function(tables) {

  # Common fixes for columns that are typically misnamed/missing
  hints <- c(SubStation = "if your data has a 'Camera' column (camtrapR output), rename it to 'SubStation'. Use NA if sites only have one camera",
             Iteration = "add an 'Iteration' column (integer, e.g. 1 for the first deployment at a site)",
             scientific_name = "run Step 5 (standardise species names) before Step 8",
             common_name = "run Step 5 (standardise species names) before Step 8",
             Latitude = "run Step 6, or supply 'Easting', 'Northing' and 'Zone' columns to be converted",
             Longitude = "run Step 6, or supply 'Easting', 'Northing' and 'Zone' columns to be converted")

  ok <- TRUE
  for (tbl_name in names(tables)) {
    present <- colnames(tables[[tbl_name]]$data)
    required <- tables[[tbl_name]]$required
    missing_cols <- setdiff(required, present)
    extra_cols <- setdiff(present, required)

    if (length(missing_cols) + length(extra_cols) == 0) next
    ok <- FALSE

    missing_txt <- vapply(missing_cols, function(col) {
      if (col %in% names(hints)) paste0("'", col, "' - ", hints[[col]]) else paste0("'", col, "'")
    }, character(1))

    cli::cli_alert_danger("Column problem in the {.strong {tbl_name}} table")
    if (length(missing_cols) > 0) {
      cli::cli_text("Missing columns - add these to the file (they can be left blank/NA where allowed):")
      cli::cli_bullets(rlang::set_names(missing_txt, rep("x", length(missing_txt))))
    }
    if (length(extra_cols) > 0) {
      cli::cli_text("Unexpected columns - remove these, or rename them to one of the missing columns above:")
      cli::cli_bullets(rlang::set_names(paste0("'", extra_cols, "'"), rep("!", length(extra_cols))))
    }
  }

  if (!ok) {
    cli::cli_text("Fix the column names in your original file(s), re-upload them, and re-run from that step. Column names are case-sensitive and must match the example data template. See weda::data_dictionary for column definitions.")
  }

  ok
}

#' Camera Trap Data Quality Checks
#' @description Assesses the data quality of camera trap records, operations and project information.
#' Automatically checks whether columns are present, converts them to the appropriate class and
#' runs a pointblank check on the data
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
                   'CameraBearing',
                   'CameraSlope',
                   'CameraID',
                   'CameraModel',
                   'CameraSensitivity',
                   'CameraPhotosPerTrigger',
                   'CameraDelay',
                   'CameraQuietPeriod',
                   'BaitedUnbaited',
                   'BaitType',
                   'BaitDistance')

  req_cols_proj <- c('ProjectName',
                     'ProjectShortName',
                     'DistanceSampling',
                     'TerrestrialArboreal',
                     'AllSpeciesTagged',
                     'DistanceForAllSpecies',
                     'ProjectDescription',
                     'ProjectLeader')

  schema_ok <- report_column_schema(
    list("Camera records (Step 2 upload)" = list(data = camtrap_records, required = req_cols),
         "Camera operation (Step 3 upload)" = list(data = camtrap_operation, required = req_cols_op),
         "Project information (Step 4 upload)" = list(data = project_information, required = req_cols_proj)))

  if(!schema_ok) {
    return(NULL)
  }

  #### Automatic Conversions ####
  # camtrap records
  message("Automatically standardising column classes, see weda::data_dictionary for database column classes")

  col_classes_recs <- weda::data_dictionary %>%
    dplyr::filter(table_name == "raw_camtrap_records") %>%
    split(., f = .$column_class)

  camtrap_records <- camtrap_records %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::any_of(col_classes_recs[["numeric"]]$column_name),
                                .fns = as.numeric),
                  dplyr::across(.cols = dplyr::any_of(col_classes_recs[["integer"]]$column_name),
                                .fns = as.integer),
                  dplyr::across(.cols = dplyr::any_of(col_classes_recs[["Date"]]$column_name),
                                .fns = as.Date),
                  dplyr::across(.cols = dplyr::any_of(col_classes_recs[["character"]]$column_name),
                                .fns = as.character),
                  dplyr::across(.cols = dplyr::any_of(col_classes_recs[["POSIXct, POSIXt"]]$column_name),
                                .fns = as.POSIXct))

  # operation records
  col_classes_op <- weda::data_dictionary %>%
    dplyr::filter(table_name == "raw_camtrap_operation") %>%
    split(., f = .$column_class)

  camtrap_operation <- camtrap_operation %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::any_of(col_classes_op[["numeric"]]$column_name),
                                .fns = as.numeric),
                  dplyr::across(.cols = dplyr::any_of(col_classes_op[["integer"]]$column_name),
                                .fns = as.integer),
                  dplyr::across(.cols = dplyr::any_of(col_classes_op[["Date"]]$column_name),
                                .fns = as.Date),
                  dplyr::across(.cols = dplyr::any_of(col_classes_op[["character"]]$column_name),
                                .fns = as.character),
                  dplyr::across(.cols = dplyr::any_of(col_classes_op[["POSIXct, POSIXt"]]$column_name),
                                .fns = as.POSIXct))

  # TimeRetrieve is optional (not always recorded at pick-up). Where the
  # retrieval date-time is missing, assume the end of the retrieval day so
  # detections on that day remain within the camera operation window
  missing_dt_retrieve <- is.na(camtrap_operation$DateTimeRetrieve) & !is.na(camtrap_operation$DateRetrieve)
  if (any(missing_dt_retrieve)) {
    tz <- attr(camtrap_operation$DateTimeRetrieve, "tzone")
    if (is.null(tz)) tz <- ""
    camtrap_operation$DateTimeRetrieve[missing_dt_retrieve] <- as.POSIXct(
      paste(camtrap_operation$DateRetrieve[missing_dt_retrieve], "23:59:59"), tz = tz)
    message(sum(missing_dt_retrieve), " operation row(s) have no retrieval time: ",
            "DateTimeRetrieve set to the end of DateRetrieve (23:59:59)")
  }

  # Project information
  col_classes_proj <- weda::data_dictionary %>%
    dplyr::filter(table_name == "raw_project_information") %>%
    split(., f = .$column_class)

  project_information <- project_information %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::any_of(col_classes_proj[["character"]]$column_name),
                                .fns = as.character),
                  dplyr::across(.cols = dplyr::any_of(col_classes_proj[["POSIXct, POSIXt"]]$column_name),
                                .fns = as.POSIXct))

  #### Poinblank checks ####

  # vba names
  vba_sci <- weda::vba_name_conversions %>%
    dplyr::filter(.data$scientific_name %in% !!camtrap_records$scientific_name)

  vba_com <- weda::vba_name_conversions %>%
    dplyr::filter(.data$common_name %in% !!camtrap_records$common_name)

  # Create Unique Iteration SiteId and SubStation
  uq_iss <- paste(camtrap_operation$Iteration,
                  camtrap_operation$SiteID,
                  camtrap_operation$SubStation, sep = "_")

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
    pointblank::col_vals_in_set("Iteration", set = camtrap_operation$Iteration) %>%
    pointblank::col_vals_in_set("Iteration_SiteID_SubStation", set = uq_iss, preconditions = ~ . %>% dplyr::mutate(Iteration_SiteID_SubStation = paste(Iteration, SiteID, SubStation, sep = "_")), label = "Combination of Iteration, SiteID, and SubStation") %>%
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
                                                      dplyr::select(dplyr::all_of(c("SiteID", "SubStation", "DateDeploy", "DateRetrieve", "Iteration"))),
                                                    by = c("SiteID", "SubStation", "Iteration"))
                                   })
# check in cases where distance is always tagged
if(project_information$DistanceSampling[1] & project_information$DistanceForAllSpecies[1])  {
  pb_rec <- pb_rec %>%
    pointblank::col_vals_not_null(c("metadata_Distance"))
}

pb_rec <- pb_rec %>%
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
    pointblank::col_vals_not_null(c('SiteID', 'Latitude', 'Longitude', 'DateDeploy', 'TimeDeploy', 'DateRetrieve', 'DateTimeDeploy', 'DateTimeRetrieve', 'CameraHeight', 'CameraID', 'Iteration', 'CameraModel',	'CameraSensitivity',	'CameraDelay',	'CameraPhotosPerTrigger', 'BaitedUnbaited', 'BaitType')) %>%
    pointblank::col_vals_in_set("BaitedUnbaited", set = c("Baited", "Unbaited")) %>%
    pointblank::col_vals_in_set("BaitType", set = c("None", "Creamed Honey", "Small Mammal Bait", "Predator Bait (i.e, meat bait)", "Non-toxic curiosity bait", "Toxic curiosity bait", "Predator Lure (i.e., urine, faeces, etc.)", "Other")) %>%
    pointblank::interrogate()

  pb_pi <- pointblank::create_agent(
    tbl = project_information,
    actions = pointblank::action_levels(stop_at = 1)) %>%
    pointblank::row_count_match(1) %>%
    pointblank::col_vals_not_null(dplyr::everything()) %>%
    pointblank::col_is_logical(c("DistanceSampling", "AllSpeciesTagged")) %>%
    pointblank::col_vals_in_set("TerrestrialArboreal", set = c("Terrestrial", "Arboreal")) %>%
    pointblank::interrogate()

  return(list(camtrap_records = pb_rec,
              camtrap_operation = pb_op,
              project_information = pb_pi))
}
