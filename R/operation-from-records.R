#' Operation from records
#'
#' @param records camera trap records from camtrapR
#'
#' @return data.frame
#' @export
camtrap_operation_from_records <- function(records) {

  setup_packdown <- records %>%
    dplyr::filter(Species %in% c("AAAA", "ZZZ")) %>%
    dplyr::group_by(Station, SubStation) %>%
    dplyr::summarise(DateTimeDeploy = max(DateTimeOriginal[Species == "AAAA"]),
                     DateTimeRetrieve = min(DateTimeOriginal[Species == "ZZZ"])) %>%
    dplyr::ungroup()
  return(setup_packdown)
}
