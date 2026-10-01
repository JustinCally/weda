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
#' @param con optional database connection. If supplied, the project short name and
#'   full name are checked against projects already on the database
#' @param schema schema to check existing projects in (camtrap or camtrap_dev)
#'
#' @return list of pointblank objects
#' @export
camera_trap_dq <- function(camtrap_records,
                           camtrap_operation,
                           project_information,
                           con = NULL,
                           schema = "camtrap") {

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

  # Keep the raw multiples so values lost in integer conversion (e.g. "3+",
  # "2.5") can be flagged; blanks/NA are allowed
  multiples_raw <- trimws(as.character(camtrap_records$metadata_Multiples))
  multiples_whole <- is.na(multiples_raw) | multiples_raw == "" | grepl("^[0-9]+$", multiples_raw)

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
    pointblank::col_exists(columns = req_cols,
      brief = "Required column is present in the camera records. If missing, add it to the records file (see the column hints above the report).") %>%
    pointblank::rows_distinct(,
      brief = "No two rows in the camera records are identical. Fails if the same image record appears more than once; remove the duplicates.") %>%
    pointblank::col_is_character(c("SiteID", "SubStation", "scientific_name", "common_name", "Time", "Directory", "FileName"),
      brief = "Column contains text. Fails if values were read as another type (e.g. numbers); check for stray formatting in the records file.") %>%
    pointblank::col_is_integer(c("Iteration", "metadata_Multiples"),
      brief = "Column contains whole numbers. Check for decimals or text such as '3+' in the records file.") %>%
    pointblank::col_vals_in_set("SiteID", set = camtrap_operation$SiteID,
      brief = "Every SiteID in the records also appears in the camera operation file. Check spelling, case and spaces match exactly.") %>%
    pointblank::col_vals_in_set("SubStation", set = camtrap_operation$SubStation,
      brief = "Every SubStation in the records also appears in the camera operation file. Check spelling, case and spaces match exactly.") %>%
    pointblank::col_vals_in_set("Iteration", set = camtrap_operation$Iteration,
      brief = "Every Iteration in the records also appears in the camera operation file.") %>%
    pointblank::col_vals_in_set("Iteration_SiteID_SubStation", set = uq_iss, preconditions = ~ . %>% dplyr::mutate(Iteration_SiteID_SubStation = paste(Iteration, SiteID, SubStation, sep = "_")), label = "Combination of Iteration, SiteID, and SubStation",
      brief = "Each record's Iteration + SiteID + SubStation combination matches a deployment in the camera operation file. Fails if a record belongs to a camera that wasn't deployed (e.g. wrong SubStation or Iteration for that site).") %>%
    pointblank::col_vals_in_set("scientific_name", set = unique(vba_sci$scientific_name),
      brief = "Scientific name matches the VBA taxa list. Fix or remove records whose species could not be matched in Step 5.") %>%
    pointblank::col_vals_in_set("common_name", set = unique(vba_com$common_name),
      brief = "Common name matches the VBA taxa list. Fix or remove records whose species could not be matched in Step 5.") %>%
    pointblank::col_vals_not_null(c("SiteID", "scientific_name", "common_name", "Date", "Time", "DateTimeOriginal", "Iteration"),
      brief = "Column has a value in every row. Fill in the missing values in the records file (unmatched species in Step 5 show up here as missing names).") %>%
    pointblank::col_vals_equal("metadata_Multiples_whole_number", value = TRUE,
                               preconditions = function(x) dplyr::mutate(x, metadata_Multiples_whole_number = multiples_whole),
                              label = "metadata_Multiples must be a whole number (or left blank)",
      brief = "metadata_Multiples is a whole number or blank. Fix entries such as '3+' or '2.5' in the image tags or records file.") %>%
    pointblank::col_is_date("Date",
      brief = "Date is a valid date. Check the date format in the records file (e.g. dd/mm/yyyy or yyyy-mm-dd).") %>%
    pointblank::col_is_posix("DateTimeOriginal",
      brief = "DateTimeOriginal is a valid date-time. Check the date-time format in the records file.") %>%
    pointblank::col_vals_between(columns = "Date",
                                 left = pointblank::vars(DateDeploy),
                                 right = pointblank::vars(DateRetrieve),
                                 inclusive = c(TRUE, TRUE),
                                 preconditions = function(x, lj = camtrap_operation) {
                                   dplyr::left_join(x, lj %>%
                                                      dplyr::select(dplyr::all_of(c("SiteID", "SubStation", "DateDeploy", "DateRetrieve", "Iteration"))),
                                                    by = c("SiteID", "SubStation", "Iteration"))
                                   },
      brief = "Each record's Date falls between the camera's deploy and retrieve dates. Fails if camera clocks were wrong or deploy/retrieve dates in the operation file are incorrect.")
# check in cases where distance is always tagged
if(project_information$DistanceSampling[1] & project_information$DistanceForAllSpecies[1])  {
  pb_rec <- pb_rec %>%
    pointblank::col_vals_not_null(c("metadata_Distance"),
      brief = "Distance is recorded for every record, because the project information says distance was tagged for all species.")
}

pb_rec <- pb_rec %>%
  pointblank::interrogate()

pb_op <- pointblank::create_agent(
    tbl = camtrap_operation,
    actions = pointblank::action_levels(stop_at = 1)) %>%
    pointblank::col_exists(columns = c('SiteID', 'SubStation', 'Iteration', 'Latitude', 'Longitude', 'DateDeploy', 'TimeDeploy', 'DateRetrieve', 'TimeRetrieve', 'Problem1_from', 'Problem1_to', 'DateTimeDeploy', 'DateTimeRetrieve', 'CameraHeight', 'CameraID', 'CameraModel',	'CameraSensitivity',	'CameraDelay',	'CameraPhotosPerTrigger'),
      brief = "Required column is present in the camera operation file. Keep every column, even if it is blank.") %>%
    pointblank::rows_distinct(,
      brief = "No two rows in the camera operation file are identical. Remove duplicate deployments.") %>%
    pointblank::col_is_character(columns = c('SiteID', 'SubStation', 'CameraID', 'CameraModel',	'CameraSensitivity',	'CameraDelay'),
      brief = "Column contains text. Check for stray formatting in the operation file.") %>%
    pointblank::col_is_numeric(columns = c('Latitude', 'Longitude', 'CameraHeight'),
      brief = "Column contains numbers. Check for text, units (e.g. '1.5m') or symbols in the operation file.") %>%
    pointblank::col_is_date(columns = c('DateDeploy', 'DateRetrieve'),
      brief = "Column is a valid date. Check the date format in the operation file (e.g. dd/mm/yyyy or yyyy-mm-dd).") %>%
    pointblank::col_is_integer(columns = c('Iteration', 'CameraPhotosPerTrigger'),
      brief = "Column contains whole numbers. Check for decimals or text in the operation file.") %>%
    pointblank::col_is_posix(columns = c('DateTimeDeploy', 'DateTimeRetrieve', 'Problem1_from', 'Problem1_to'),
      brief = "Column is a valid date-time (or blank where allowed). Check the date-time format in the operation file.") %>%
    pointblank::col_vals_in_set(columns = c('SiteID'), set = camtrap_records$SiteID, actions = pointblank::action_levels(stop_at = 0.99, warn_at = 1),
      brief = "Each SiteID in the operation file has at least one record. A warning only: cameras with no detections are fine, but check for SiteID typos.") %>%
    pointblank::col_vals_in_set(columns = c('SubStation'), set = camtrap_records$SubStation, actions = pointblank::action_levels(stop_at = 0.99, warn_at = 1),
      brief = "Each SubStation in the operation file has at least one record. A warning only: cameras with no detections are fine, but check for typos.") %>%
    pointblank::col_vals_between(columns = c('Latitude'), left = -60.55, right = -8.47,
      brief = "Latitude is within Australia (decimal degrees). Check latitude/longitude aren't swapped and coordinates aren't in eastings/northings.") %>%
    pointblank::col_vals_between(columns = c('Longitude'), left = 93.41, right = 173.34,
      brief = "Longitude is within Australia (decimal degrees). Check latitude/longitude aren't swapped and coordinates aren't in eastings/northings.") %>%
    pointblank::col_vals_not_null(c('SiteID', 'Latitude', 'Longitude', 'DateDeploy', 'TimeDeploy', 'DateRetrieve', 'DateTimeDeploy', 'DateTimeRetrieve', 'CameraHeight', 'CameraID', 'Iteration', 'CameraModel',	'CameraSensitivity',	'CameraDelay',	'CameraPhotosPerTrigger', 'BaitedUnbaited', 'BaitType'),
      brief = "Column has a value in every row. Fill in the missing values in the operation file.") %>%
    pointblank::col_vals_in_set("BaitedUnbaited", set = c("Baited", "Unbaited"),
      brief = "BaitedUnbaited is either 'Baited' or 'Unbaited'.") %>%
    pointblank::col_vals_in_set("BaitType", set = c("None", "Creamed Honey", "Small Mammal Bait", "Predator Bait (i.e, meat bait)", "Non-toxic curiosity bait", "Toxic curiosity bait", "Predator Lure (i.e., urine, faeces, etc.)", "Other"),
      brief = "BaitType is one of the allowed options (see the example data template), e.g. 'None' for unbaited cameras.") %>%
    pointblank::interrogate()

  # Project names must match any existing project exactly: the project
  # database ID is derived from ProjectName
  project_names_ok <- check_project_names(project_information, con = con, schema = schema)

  pb_pi <- pointblank::create_agent(
    tbl = project_information,
    actions = pointblank::action_levels(stop_at = 1)) %>%
    pointblank::row_count_match(1,
      brief = "The project information file has exactly one row.") %>%
    pointblank::col_vals_not_null(dplyr::everything(),
      brief = "Every project information column is filled in.") %>%
    pointblank::col_is_logical(c("DistanceSampling", "AllSpeciesTagged"),
      brief = "Column is TRUE or FALSE.") %>%
    pointblank::col_vals_in_set("TerrestrialArboreal", set = c("Terrestrial", "Arboreal"),
      brief = "TerrestrialArboreal is either 'Terrestrial' or 'Arboreal'.") %>%
    pointblank::col_vals_equal("ProjectNamesMatchDatabase", value = TRUE,
      preconditions = function(x) dplyr::mutate(x, ProjectNamesMatchDatabase = project_names_ok),
      label = "ProjectShortName and ProjectName match existing projects on the database",
      brief = "If the ProjectShortName or ProjectName is already on the database, the other name must match that project exactly (see the message above the report). Use the existing names to add data to that project, or new names for a new project.") %>%
    pointblank::interrogate()

  return(list(camtrap_records = pb_rec,
              camtrap_operation = pb_op,
              project_information = pb_pi))
}


#' Check project names against existing projects on the database
#'
#' @param project_information project information data.frame (one row)
#' @param con database connection (NULL skips the check)
#' @param schema schema to check
#'
#' @noRd
#'
#' @return TRUE if the names are consistent with the database (or the check was skipped)
check_project_names <- function(project_information, con = NULL, schema = "camtrap") {

  short_name <- trimws(as.character(project_information$ProjectShortName[1]))
  full_name <- trimws(as.character(project_information$ProjectName[1]))

  if (is.null(con)) {
    cli::cli_alert_warning("Project names not checked against existing projects (no database connection)")
    return(TRUE)
  }

  existing <- tryCatch(existing_projects(con, schema, short_name, full_name),
                       error = function(e) e)
  if (inherits(existing, "error")) {
    cli::cli_alert_warning("Could not check project names against the database ({schema}): {conditionMessage(existing)}")
    return(TRUE)
  }

  if (nrow(existing) == 0) {
    cli::cli_alert_info("New project: '{short_name}' ({full_name}) will be created on upload")
    return(TRUE)
  }

  if (any(existing$ProjectShortName == short_name & existing$ProjectName == full_name)) {
    cli::cli_alert_info("Existing project: data will be added to '{short_name}' ({full_name})")
    return(TRUE)
  }

  cli::cli_alert_danger("Project names don't match the database ({schema})")
  same_short <- existing[existing$ProjectShortName == short_name, ]
  same_full <- existing[existing$ProjectName == full_name, ]
  for (i in seq_len(nrow(same_short))) {
    cli::cli_bullets(c("x" = "ProjectShortName '{short_name}' already exists with ProjectName '{same_short$ProjectName[i]}' (yours: '{full_name}')"))
  }
  for (i in seq_len(nrow(same_full))) {
    cli::cli_bullets(c("x" = "ProjectName '{full_name}' already exists with ProjectShortName '{same_full$ProjectShortName[i]}' (yours: '{short_name}')"))
  }
  cli::cli_text("To add to the existing project, copy both names exactly as they are on the database. For a new project, use a new ProjectShortName and ProjectName.")
  FALSE
}

#' Projects on the database sharing a short or full name
#'
#' @noRd
existing_projects <- function(con, schema, short_name, full_name) {
  dplyr::tbl(con, dbplyr::in_schema(schema, "raw_project_information")) %>%
    dplyr::filter(.data$ProjectShortName %in% !!short_name | .data$ProjectName %in% !!full_name) %>%
    dplyr::select(dplyr::all_of(c("ProjectShortName", "ProjectName"))) %>%
    dplyr::distinct() %>%
    dplyr::collect()
}
