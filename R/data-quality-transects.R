data_quality_transects <- function(records, transects, project_information) {

  req_cols <- c('SiteID' ,
                'Transect',
                'Iteration',
                'scientific_name',
                'common_name',
                'SurveyMethod',
                'DateTime',
                'AnimalID',
                'SeenHeard',
                'Adults',
                'Joeys',
                'Individuals',
                'LoR',
                'WaypointNo',
                'ObserverLatitude',
                'ObserverLongitude',
                'AnimalDistance',
                'AnimalHeight',
                "AnimalHorizontalDistance",
                "AnimalAngle",
                "AnimalBearing",
                "DistanceFromTransectStart",
                "TreeSpecies",
                "BothSeen",
                "ObservationNotes",
                "ObserverPosition",
                "AnimalLongitude",
                "AnimalLatitude",
                "ColourForm",
                "PhotoID",
                "AnimalLongitude2",
                "AnimalLatitude2",
                "LoR2",
                "SeenOnSameSide",
                "DistanceBetweenAnimalProj",
                "AnimalPerpDistance")

  req_cols_op <- c('SiteID',
                   'Transect',
                   'Iteration',
                   'ObserverPosition',
                   'ObserverName',
                   'ObserverID',
                   'Date',
                   'StartTime',
                   'EndTime',
                   'Duration',
                   'Weather',
                   'Temperature',
                   'TransectNotes',
                   "MoonPhase",
                   "Cloud",
                   "RelativeHumidity",
                   "Wind",
                   "Precipitation",
                   "FlowerIndex",
                   "Access",
                   "Visibility",
                   'TransectLength',
                   'MaxTruncationDistance',
                   'TransectType',
                   'geometry')

  req_cols_proj <- c('ProjectName',
                     'ProjectShortName',
                     'DistanceSampling',
                     'TerrestrialArboreal',
                     'AllSpeciesTagged',
                     'DistanceForAllSpecies',
                     'DiurnalNocturnal',
                     'ProjectDescription',
                     'ProjectLeader')

  c1 <- colnames(records)
  c2 <- colnames(transects)
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
    cli::cli_alert_danger(c("Problem with column schema. ",
                            "Please correct (add/remove/rename) the following columns:\n ◉ ",
                            paste(unlist(difflist), collapse = "\n ◉ "),
                            "\n",
                            "See weda::data_dictionary for more information on user inputs"))
    return(NULL)
  }

  # vba names
  vba_sci <- weda::vba_name_conversions %>%
    dplyr::filter(.data$scientific_name %in% !!records$scientific_name)

  vba_com <- weda::vba_name_conversions %>%
    dplyr::filter(.data$common_name %in% !!records$common_name)

  # Create Unique Iteration SiteId and SubStation
  uq_iss <- paste(transects$Iteration,
                  transects$SiteID,
                  transects$Transect, sep = "_")

  # Create a pointblank object
  pb_rec <- pointblank::create_agent(
    tbl = records,
    actions = pointblank::action_levels(stop_at = 1)) %>%
    pointblank::col_exists(columns = req_cols) %>%
    pointblank::rows_distinct() %>%
    pointblank::col_is_character(c("SiteID", "scientific_name", "common_name", "SeenHeard", "LoR", "WaypointNo", "TreeSpecies", "ObservationNotes", "LoR2", "SurveyMethod", "ColourForm", "PhotoID", "AnimalID")) %>%
    pointblank::col_is_integer(c("Iteration", "Transect", "Adults", "Joeys", "Individuals", "ObserverPosition"))  %>%
    pointblank::col_is_numeric(c("ObserverLatitude", "ObserverLongitude", "AnimalDistance", "AnimalHeight", "AnimalHorizontalDistance", "AnimalAngle", "AnimalBearing","DistanceFromTransectStart", "AnimalLongitude", "AnimalLatitude", "AnimalLongitude2", "AnimalLatitude2", "DistanceBetweenAnimalProj", "AnimalPerpDistance"))  %>%
    pointblank::col_vals_in_set("SiteID", set = transects$SiteID) %>%
    pointblank::col_vals_in_set("Transect", set = transects$Transect) %>%
    pointblank::col_vals_in_set("Iteration", set = transects$Iteration) %>%
    pointblank::col_vals_between(columns = c('ObserverLatitude', 'AnimalLatitude'), left = -60.55, right = -8.47) %>%
    pointblank::col_vals_between(columns = c('ObserverLongitude', 'AnimalLongitude'), left = 93.41, right = 173.34) %>%
    pointblank::col_vals_in_set("Iteration_SiteID_Transect", set = uq_iss, preconditions = ~ . %>% dplyr::mutate(Iteration_SiteID_Transect = paste(Iteration, SiteID, Transect, sep = "_")), label = "Combination of Iteration, SiteID, and Transect") %>%
    pointblank::col_vals_in_set("scientific_name", set = unique(vba_sci$scientific_name)) %>%
    pointblank::col_vals_in_set("common_name", set = unique(vba_com$common_name)) %>%
    pointblank::col_vals_in_set("SeenHeard", set = c("Seen", "Heard", "Other - define in comments")) %>%
    pointblank::col_vals_in_set("SurveyMethod", set = c("Diurnal double-observer distance-sampling",
                                                        "Spotlight double-observer distance-sampling",
                                                        "Thermal double-observer distance-sampling",
                                                        "Diurnal single-observer distance-sampling",
                                                        "Spotlight single-observer distance-sampling",
                                                        "Thermal single-observer distance-sampling",
                                                        "Thermal detection",
                                                        "Spotlight detection",
                                                        "Spotlight/call-playback detection",
                                                        "Owl call-playback",
                                                        "Recce",
                                                        "Diurnal bird survey",
                                                        "Diurnal bird survey (with call playback)",
                                                        "Diurnal drone survey",
                                                        "Nocturnal drone survey")) %>%
    pointblank::col_vals_not_null(c("SiteID", "scientific_name", "common_name", "DateTime", "Iteration", "Individuals", "ObserverLatitude", "ObserverLongitude", "ObserverPosition")) %>%
    pointblank::col_is_posix("DateTime") %>%
    pointblank::col_vals_between(columns = "DateTime",
                                 left = pointblank::vars(StartTime),
                                 right = pointblank::vars(EndTime),
                                 inclusive = c(TRUE, TRUE),
                                 preconditions = function(x, lj = transects) {
                                   dplyr::left_join(x, lj %>%
                                                      dplyr::select(dplyr::all_of(c("SiteID", "Transect", "StartTime", "EndTime", "Iteration", "ObserverPosition"))),
                                                    by = c("SiteID", "Transect", "Iteration", "ObserverPosition"))
                                 })

  # check in cases where distance is always tagged
  if(project_information$DistanceSampling[1] & project_information$DistanceForAllSpecies[1])  {
    pb_rec <- pb_rec %>%
      pointblank::col_vals_not_null(c("AnimalPerpDistance", "AnimalHorizontalDistance", "AnimalBearing"))
  }

  pb_rec <- pb_rec %>%
    pointblank::interrogate()

  # check sf specifics
  t_class <- class(transects)
  if(!("sf" %in% t_class)) {
    cli::cli_alert_danger("Transects are not an sf geometry object")
    return(NULL)
  }
  t_valid <- all(sf::st_is_valid(transects))
  if(!t_valid) {
    cli::cli_alert_danger("Transects are not valid according to sf geometry. Use sf::st_make_valid()")
    return(NULL)
  }

  pb_op <- pointblank::create_agent(
    tbl = transects,
    actions = pointblank::action_levels(stop_at = 1)) %>%
    pointblank::col_exists(columns = c('SiteID',
                                       'Transect',
                                       'Iteration',
                                       'ObserverPosition',
                                       'ObserverName',
                                       'Date',
                                       'StartTime',
                                       'EndTime',
                                       'Duration',
                                       'Weather',
                                       'Temperature',
                                       'TransectNotes',
                                       "MoonPhase",
                                       "Cloud",
                                       "RelativeHumidity",
                                       "Wind",
                                       "Precipitation",
                                       "FlowerIndex",
                                       "Access",
                                       "Visibility",
                                       'TransectLength',
                                       'TransectType',
                                       'MaxTruncationDistance',
                                       'geometry')) %>%
    pointblank::rows_distinct() %>%
    pointblank::col_is_character(columns = c('SiteID', 'ObserverID', 'ObserverName',	'Weather',	"TransectNotes", "MoonPhase", "Wind", "Precipitation", "FlowerIndex", "Access", "Visibility",'TransectType')) %>%
    pointblank::col_is_numeric(columns = c('TransectLength', 'MaxTruncationDistance')) %>%
    pointblank::col_is_date(columns = c('Date')) %>%
    pointblank::col_is_integer(columns = c('Iteration', 'ObserverPosition', 'Transect')) %>%
    pointblank::col_is_posix(columns = c('StartTime', 'EndTime')) %>%
    pointblank::col_vals_in_set(columns = c('SiteID'), set = records$SiteID, actions = pointblank::action_levels(stop_at = 0.99, warn_at = 1)) %>%
    pointblank::col_vals_in_set(columns = c('Transect'), set = records$Transect, actions = pointblank::action_levels(stop_at = 0.99, warn_at = 1)) %>%
    pointblank::col_vals_not_null(c('SiteID', 'Transect', 'Date', 'StartTime', 'EndTime', 'ObserverID', 'ObserverName', 'Iteration', 'Duration', 'TransectLength', 'MaxTruncationDistance', 'TransectType', 'geometry')) %>%
    pointblank::col_vals_in_set("Visibility", set = c("Poor", "Moderate", "Excellent"),
                                preconditions = ~ . %>% dplyr::filter(!is.na(Visibility))) %>%
    pointblank::col_vals_in_set("TransectType", set = c("Line", "Point")) %>%
    pointblank::col_vals_in_set("FlowerIndex", set = c("No trees in flower",
                                                       "Light flowering",
                                                       "Medium flowering",
                                                       "Heavy flowering"),
                                preconditions = ~ . %>% dplyr::filter(!is.na(FlowerIndex))) %>%
    pointblank::col_vals_in_set("GeometryType", set = c("LINESTRING", "MULTILINESTRING"),
                                preconditions = ~ . %>%
                                  mutate(GeometryType = sf::st_geometry_type(geometry)) %>%
                                  filter(TransectType == "Line")) %>%
    pointblank::col_vals_in_set("GeometryType", set = c("POINT", "MULTIPOINT"),
                                preconditions = ~ . %>%
                                  mutate(GeometryType = sf::st_geometry_type(geometry)) %>%
                                  filter(TransectType == "Point")) %>%
    pointblank::interrogate()

  pb_pi <- pointblank::create_agent(
    tbl = project_information,
    actions = pointblank::action_levels(stop_at = 1)) %>%
    pointblank::row_count_match(1) %>%
    pointblank::col_vals_not_null(dplyr::everything()) %>%
    pointblank::col_is_logical(c("DistanceSampling", "AllSpeciesTagged", "DistanceForAllSpecies")) %>%
    pointblank::col_vals_in_set("TerrestrialArboreal", set = c("Terrestrial", "Arboreal", "Both")) %>%
    pointblank::col_vals_in_set("DiurnalNocturnal", set = c("Diurnal", "Nocturnal", "Both")) %>%
    pointblank::interrogate()


  return(list(camtrap_records = pb_rec,
              camtrap_operation = pb_op,
              project_information = pb_pi))

}
