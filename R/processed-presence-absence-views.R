#' Presence/Absence Views
#'
#' @description presence-absence data views for each site/substation (including a daily presence-absence)
#'
#' @param con database connection
#' @param return_data logical flag to return data (TRUE) or sql query (default is FALSE)
#' @param daily logical flag to make the query/data be at a daily level (TRUE), or per substation (FALSE)
#'
#' @return sql or data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' DBI::dbExecute(conn = con_odbc,
#'                paste(SQL("CREATE VIEW test.processed_site_substation_presence_absence AS"),
#'                processed_SubStation_presence_absence(con = con_odbc, return_data = FALSE)))
#' }
processed_SubStation_presence_absence <- function(con, return_data = FALSE, daily = FALSE) {

  curated_camtrap_records <- dplyr::tbl(con, dbplyr::in_schema("test", "curated_camtrap_records"))
  curated_camtrap_operation <- dplyr::tbl(con, dbplyr::in_schema("test", "curated_camtrap_operation"))

  species_real <- curated_camtrap_records %>%
    dplyr::mutate(SiteStation = paste(SiteID, SubStation, sep = "_")) %>%
    dplyr::select(dplyr::all_of(c("ProjectShortName",
                                  "SiteStation",
                                  "scientific_name",
                                  "common_name",
                                  if(daily) "Date"))) %>%
    dplyr::distinct()

  species_project_possible <- curated_camtrap_records %>%
    dplyr::mutate(SiteStation = paste(SiteID, SubStation, sep = "_")) %>%
    dplyr::group_by(ProjectShortName) %>%
    tidyr::expand(tidyr::nesting(SiteStation, SiteID, SubStation),
                  tidyr::nesting(scientific_name, common_name)) %>%
    dplyr::ungroup()

  join_cols <- c("ProjectShortName", "SiteStation", "scientific_name", "common_name")

  if(daily) {
    date_sites <- curated_camtrap_operation %>%
      dplyr::mutate(SiteStation = paste(SiteID, SubStation, sep = "_"),
                    DateEnd = dplyr::coalesce(as.Date(Problem1_from), DateRetrieve)) %>%
      dplyr::group_by(ProjectShortName, SiteStation) %>%
      dplyr::transmute(Date = generate_series(DateDeploy, DateEnd, '1 day'))

    species_project_possible <- species_project_possible %>%
      dplyr::right_join(date_sites, by = c("ProjectShortName", "SiteStation"))

    join_cols <- c(join_cols, "Date")
  }

  species_occ <- species_real %>%
    dplyr::mutate(Presence = 1) %>%
    dplyr::right_join(species_project_possible, by = join_cols) %>%
    dplyr::mutate(Presence = dplyr::coalesce(Presence, 0)) %>%
    dplyr::select(dplyr::all_of(c("ProjectShortName",
                                  "SiteID",
                                  "SubStation",
                                  if(daily) "Date",
                                  "scientific_name",
                                  "common_name",
                                  "Presence")))

  if(return_data) {
    return(dplyr::collect(species_occ))
  } else {
    return(dbplyr::remote_query(species_occ))
  }

}
