#' Operation from records
#'
#' @param records camera trap records from camtrapR  with Species, SiteID and SubStation
#' @param get_cam_model logical flag (default is TRUE) of whether you want to get cam model and serial number (drive with photos needs to be attached)
#'
#' @return data.frame
#' @export
#' @examples
#' library(weda)
#' raw_camtrap_records <- camtrapR::recordTable(inDir  = system.file("dummydata/images",
#' package = "weda"),
#'                                              IDfrom = "metadata",
#'                                              cameraID = "directory",
#'                                              stationCol = "SiteID",
#'                                              camerasIndependent = TRUE,
#'                                              timeZone = Sys.timezone(location = TRUE),
#'                                              metadataSpeciesTag = "Species",
#'                                              removeDuplicateRecords = FALSE,
#'                                              returnFileNamesMissingTags = TRUE) %>%
#' dplyr::rename(SubStation = Camera) %>%
#' dplyr::mutate(SubStation = dplyr::case_when(SiteID == SubStation ~ NA_character_,
#'                                             TRUE ~ SubStation),
#'               Iteration = 1L)
#'
#'  op_table <- camtrap_operation_from_records(raw_camtrap_records)
#'
camtrap_operation_from_records <- function(records, get_cam_model = TRUE) {

  setup_packdown <- records %>%
    dplyr::group_by(SiteID, SubStation, Iteration) %>%
    dplyr::summarise(DateTimeDeploy = min(DateTimeOriginal),
                     DateTimeRetrieve = max(DateTimeOriginal)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(TimeDeploy = format(DateTimeDeploy, format="%H:%M:%S"),
                  TimeRetrieve = format(DateTimeRetrieve, format="%H:%M:%S"))

  if(!get_cam_model) {
    return(setup_packdown)
  } else{

  files_to_extract <- records %>%
    dplyr::group_by(SiteID, SubStation, Iteration) %>%
    dplyr::slice(1) %>%
    dplyr::mutate(FileFullPath = file.path(Directory, FileName)) %>%
    dplyr::select(SiteID, SubStation, Iteration, FileFullPath)

  tags <- vector("list", length = nrow(files_to_extract))

  for(i in 1:nrow(files_to_extract)) {
    tags[[i]] <- camtrapR::exifTagNames(fileName = files_to_extract$FileFullPath[i]) %>%
      dplyr::filter(tag_name %in% c("UserLabel", "SerialNumber")) %>%
      dplyr::select(-tag_group) %>%
      tidyr::pivot_wider(names_from = tag_name, values_from = value) %>%
      dplyr::rename(CameraModel = UserLabel, CameraID = SerialNumber)
  }

  tags_bound <- dplyr::bind_rows(tags)

  return_data <- dplyr::bind_cols(setup_packdown, tags_bound)

  return(return_data)
  }
}
