#' Operation from records
#'
#' @param records camera trap records from camtrapR  with Species, SiteID and SubStation
#' @param start_tag Species tag for camera deploy (default is "AAAA")
#' @param end_tag Species tag for camera pickup (default is "AAAA")
#'
#' @return data.frame
#' @export
camtrap_operation_from_records <- function(records,
                                           start_tag = "AAAA",
                                           end_tag = "ZZZ") {

  setup_packdown <- records %>%
    dplyr::filter(Species %in% c("AAAA", "ZZZ")) %>%
    dplyr::group_by(SiteID, SubStation, Iteration) %>%
    dplyr::summarise(DateTimeDeploy = max(DateTimeOriginal[Species == "AAAA"]),
                     DateTimeRetrieve = min(DateTimeOriginal[Species == "ZZZ"])) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(TimeDeploy = format(DateTimeDeploy, format="%H:%M:%S"),
                  TimeRetrieve = format(DateTimeRetrieve, format="%H:%M:%S"))

  files_to_extract <- records %>%
    dplyr::group_by(SiteID, SubStation, Iteration) %>%
    dplyr::slice(1) %>%
    dplyr::mutate(FileFullPath = file.path(Directory, FileName)) %>%
    dplyr::select(SiteID, SubStation, Iteration, FileFullPath)

  tags <- vector("list", length = nrow(files_to_extract))

  for(i in 1:nrow(files_to_extract)) {
    tags[[i]] <- camtrapR::exifTagNames(fileName = files_to_extract$FileFullPath[i]) %>%
      dplyr::filter(tag_name %in% c("Model", "SerialNumber")) %>%
      dplyr::select(-tag_group) %>%
      tidyr::pivot_wider(names_from = tag_name, values_from = value) %>%
      dplyr::rename(CameraModel = Model, CameraID = SerialNumber)
  }

  tags_bound <- dplyr::bind_rows(tags)

  return_data <- dplyr::bind_cols(setup_packdown, tags_bound)

  return(return_data)
}
#
# raw_camtrap_records <- recordTable(inDir  = system.file("dummydata/images", package = "weda"),
#                                    IDfrom = "metadata",
#                                    cameraID = "directory",
#                                    stationCol = "SiteID",
#                                    camerasIndependent = TRUE,
#                                    timeZone = Sys.timezone(location = TRUE),
#                                    metadataSpeciesTag = "Species",
#                                    removeDuplicateRecords = FALSE,
#                                    returnFileNamesMissingTags = TRUE) %>%
#   rename(SubStation = Camera) %>%
#   mutate(SubStation = case_when(SiteID == SubStation ~ NA_character_,
#                                 TRUE ~ SubStation),
#          Iteration = 1L) %>%
#   dplyr::filter(SiteID %in% c("56505", "9941")) %>%
#   mutate(Species = c("AAAA", "ZZZ", "AAAA", "ZZZ"),
#          SubStation = NA_character_)
#
# camtrap_operation_from_records(raw_camtrap_records)
