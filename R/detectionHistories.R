#' Database to detion histories
#'
#' @param con database connection string (as returned via weda_connect())
#' @param ProjectShortName Project short name (single value/project must be provided)
#' @param Species Common species name/s to obtain detection histories for (e.g. Leadbeater's Possum)
#' @param Iteration Iteration of survey (only single value allowed, loop function if multiple iterations required)
#' @param byCamera passed to camtrapR: logical. If TRUE, camera operability matrix is computed by camera, not by station (requires cameraCol)
#' @param occasionStartTime passed to camtrapR: integer. time of day (the full hour) at which to begin occasions. Replaces occasionStartTime
#' from detectionHistory and spatialDetectionHistory.
#' @param camerasIndependent passed to camtrapR: logical. Return number of active camera traps by station? Only if byCamera
#' is FALSE and allCamsOn is FALSE. If camerasIndependent is TRUE, output values will be the number of operational cameras
#' at a station. If camerasIndependent is FALSE, the value is 1 if at least 1 camera was operational, otherwise 0. In both cases,
#' values are NA if no camera was set up.
#' @param output passed to camtrapR: character. Return binary detections ("binary") or counts of detections ("count")
#' @param occasionLength passed to camtrapR: 	integer. occasion length in days
#' @param includeEffort passed to camtrapR: logical. Compute trapping effort (number of active camera trap days per station and occasion)?
#' @param day1 passed to camtrapR: character. When should occasions begin: station setup date ("station"), first day of survey ("survey"), a specific date (e.g. "2015-12-31")?
#' @param ... additional arguments passed to \code{camtrapR::detectionHistory()}
#'
#' @return Depending on the value of \code{includeEffort} and
#' \code{scaleEffort}, a list with either 1, 2 or 3 elements. The first element
#' is the species detection history. The second is the optional effort matrix
#' and the third contains the effort scaling parameters.
#' \item{detection_history}{A species detection matrix} \item{effort}{A matrix
#' giving the number of active camera trap days per station and occasion (=
#' camera trapping effort). It is only returned if \code{includeEffort = TRUE}}
#' \item{effort_scaling_parameters}{Scaling parameters of the effort matrix. It
#' is only returned if \code{includeEffort} and \code{scaleEffort} are
#' \code{TRUE}}
#' @export
#'
#' @examples
#'
#' \dontrun{
#'con <- weda::weda_connect(password = keyring::key_get(service = "ari-dev-weda-psql-01",
#'                                                      username = "psql_user"), username = "psql_user")
#'
#'lbp_det_hist <- DBdetectionHistory(con,
#'                                   ProjectShortName = "FPSP_LBPCT",
#'                                   Species = "Leadbeater's Possum",
#'                                   Iteration = 1,
#'                                   byCamera = TRUE,
#'                                   occasionStartTime = 0,
#'                                   camerasIndependent = TRUE,
#'                                   output = "binary",
#'                                   occasionLength = 1,
#'                                   includeEffort = FALSE,
#'                                   scaleEffort = FALSE,
#'                                   day1 = "station")
#'                                   }
DBdetectionHistory <- function(con,
                               ProjectShortName,
                               Species,
                               Iteration = 1,
                               byCamera,
                               occasionStartTime = 0,
                               camerasIndependent = TRUE,
                               output = "binary",
                               occasionLength = 1,
                               includeEffort = TRUE,
                               day1 = "station",
                               ...) {

  if(length(ProjectShortName) > 1) {
    stop("Can only determine detection history for 1 project at a time")
  }

# records
  message("Downloading camera trap records")
camtrap_records <- dplyr::tbl(con, dbplyr::in_schema("camtrap", "curated_camtrap_records")) %>%
    dplyr::filter(ProjectShortName == !!ProjectShortName, common_name %in% Species & Iteration == Iteration) %>%
    dplyr::collect() %>%
  dplyr::mutate(DateTimeOriginal = lubridate::ymd_hms(DateTimeOriginal, tz = "Australia/Queensland"),
                # DateTimeOriginal = dplyr::case_when(format(DateTimeOriginal, "%H:%M:%S") == "00:00:00" ~ DateTimeOriginal + lubridate::seconds(1),
                #                                     TRUE ~ DateTimeOriginal),
                DateTimeOriginal = lubridate::parse_date_time(DateTimeOriginal,
                                                              tz = "Australia/Queensland",
                                                              orders = "ymd HMS"),
         Date = as.Date(DateTimeOriginal, tz = "Australia/Queensland")) %>%
  dplyr::filter(!is.na(DateTimeOriginal))

# operation
message("Downloading camera trap operation")
camtrap_operation <- dplyr::tbl(con, dbplyr::in_schema("camtrap", "curated_camtrap_operation")) %>%
  dplyr::filter(ProjectShortName == !!ProjectShortName & Iteration == Iteration) %>%
  dplyr::collect()

camtrap_operation_fix <- camtrap_operation %>%
  dplyr::rowwise() %>%
  dplyr::mutate(DateTimeRetrieve = lubridate::ymd_hms(paste(DateRetrieve, TimeRetrieve), tz = "Australia/Queensland"),
                DateTimeDeploy = lubridate::ymd_hms(paste(DateDeploy, TimeDeploy), tz = "Australia/Queensland"),
                DateTimeDeploy = dplyr::case_when(format(DateTimeDeploy, "%H:%M:%S") == "00:00:00" ~ DateTimeDeploy + lubridate::seconds(1),
                                                  TRUE ~ DateTimeDeploy),
                DateTimeRetrieve = dplyr::case_when(format(DateTimeRetrieve, "%H:%M:%S") == "00:00:00" ~ DateTimeRetrieve + lubridate::seconds(1),
                                                  TRUE ~ DateTimeRetrieve),
                DateTimeRetrieve = dplyr::case_when(DateTimeRetrieve == DateTimeDeploy ~ DateTimeRetrieve + lubridate::seconds(1),
                                                    TRUE ~ DateTimeRetrieve),
                Problem1_from = lubridate::ymd_hms(Problem1_from, tz = "Australia/Queensland"),
                Problem1_to = lubridate::ymd_hms(Problem1_to, tz = "Australia/Queensland"),
                Problem1_from = lubridate::ymd_hms(max(Problem1_from, DateTimeDeploy, na.rm = F), tz = "Australia/Queensland"),
                Problem1_to = lubridate::ymd_hms(min(Problem1_to, DateTimeRetrieve, na.rm = F), tz = "Australia/Queensland"),
                DateDeploy =as.Date(DateTimeDeploy),
                DateRetrieve =as.Date(DateTimeRetrieve)) %>%
  dplyr::ungroup()

if(all(is.na(camtrap_operation_fix$Problem1_from))) {
  hp <- FALSE
} else {
  hp <- TRUE
}

if(byCamera) {
  opmat <- camtrapR::cameraOperation(camtrap_operation_fix,
                                   stationCol = "SiteID",
                                   cameraCol = "SubStation",
                                   setupCol = "DateTimeDeploy",
                                   retrievalCol = "DateTimeRetrieve",
                                   hasProblems = hp,
                                   byCamera = byCamera,
                                   dateFormat = "ymd HMS",
                                   allCamsOn = TRUE,
                                   camerasIndependent = camerasIndependent,
                                   occasionStartTime = occasionStartTime)
} else {
  opmat <- camtrapR::cameraOperation(camtrap_operation_fix,
                                     stationCol = "SiteID",
                                     setupCol = "DateTimeDeploy",
                                     retrievalCol = "DateTimeRetrieve",
                                     hasProblems = hp,
                                     byCamera = FALSE,
                                     dateFormat = "ymd HMS",
                                     allCamsOn = TRUE,
                                     camerasIndependent = camerasIndependent,
                                     occasionStartTime = occasionStartTime)
}

if(byCamera) {
  camtrap_records <- camtrap_records %>%
    dplyr::mutate(SiteStation = paste0(SiteID, "__CAM_", SubStation))
} else {
  camtrap_records <- camtrap_records %>%
    dplyr::mutate(SiteStation = SiteID)
}

detection_history <- camtrapR::detectionHistory(recordTable = camtrap_records,
                                      camOp = opmat,
                                      species = Species,
                                      output = output,
                                      stationCol = "SiteStation",
                                      speciesCol = "common_name",
                                      occasionLength = occasionLength,
                                      includeEffort = includeEffort,
                                      day1 = day1,
                                      recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                                      timeZone = "Australia/Queensland",
                                      ...)

return(detection_history)

}
