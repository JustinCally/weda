#' Presence/Absence Views
#'
#' @description presence-absence data views for each site/substation (including a daily presence-absence)
#'
#' @param con database connection
#' @param return_data logical flag to return data (TRUE) or sql query (default is FALSE)
#' @param daily logical flag to make the query/data be at a daily level (TRUE), or per substation (FALSE)
#' @param species whether to make table for 'all' species (default), otherwise accepts a vector of common names (see vba_name_conversions)
#'
#' @return sql or data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' con <- weda_connect(password = keyring::key_get(service = "ari-dev-weda-psql-01", username = "psql_user"))
#' DBI::dbExecute(conn = con,
#'                paste(SQL("CREATE VIEW camtrap.processed_site_substation_presence_absence AS"),
#'                processed_SubStation_presence_absence(con = con, return_data = FALSE)))
#' }
processed_SubStation_presence_absence <- function(con,
                                                  return_data = FALSE,
                                                  daily = FALSE,
                                                  species = "all") {

  curated_camtrap_records <- dplyr::tbl(con, dbplyr::in_schema("camtrap", "curated_camtrap_records"))

  if(species != "all") {
    curated_camtrap_records <- curated_camtrap_records %>%
      dplyr::filter(common_name %in% species)
  }

  curated_camtrap_operation <- dplyr::tbl(con, dbplyr::in_schema("camtrap", "curated_camtrap_operation"))

  species_real <- curated_camtrap_records %>%
    dplyr::mutate(SiteStation = paste(SiteID, SubStation, sep = "_")) %>%
    dplyr::select(dplyr::all_of(c("ProjectShortName",
                                  "SiteStation",
                                  "Iteration",
                                  "scientific_name",
                                  "common_name",
                                  if(daily) "Date"))) %>%
    dplyr::distinct()


  join_cols <- c("ProjectShortName", "SiteStation", "Iteration", "scientific_name", "common_name")

  species_project_possible <- curated_camtrap_records %>%
    dplyr::mutate(SiteStation = paste(SiteID, SubStation, sep = "_")) %>%
    dplyr::select(-SubStation) %>%
    dplyr::full_join(curated_camtrap_operation %>%
                       dplyr::mutate(SiteStation = paste(SiteID, SubStation, sep = "_")) %>%
                       dplyr::select(ProjectShortName, SiteID, SiteStation, SubStation, Iteration),
                                     by = c("ProjectShortName", "SiteID", "SiteStation", "Iteration")) %>%
    dplyr::group_by(ProjectShortName) %>%
    tidyr::expand(tidyr::nesting(SiteStation, SiteID, SubStation, Iteration),
                  tidyr::nesting(scientific_name, common_name)) %>%
    dplyr::ungroup()


  if(daily) {
    date_sites <- curated_camtrap_operation %>%
      dplyr::mutate(SiteStation = paste(SiteID, SubStation, sep = "_"),
                    DateEnd = dplyr::coalesce(as.Date(Problem1_from), DateRetrieve)) %>%
      dplyr::group_by(ProjectShortName, SiteStation, Iteration) %>%
      dplyr::transmute(Date = generate_series(DateDeploy, DateEnd, '1 day'))

    species_project_possible <- species_project_possible %>%
      dplyr::right_join(date_sites, by = c("ProjectShortName", "SiteStation", "Iteration"))

    join_cols <- c(join_cols, "Date")
  }

  species_occ <- species_real %>%
    dplyr::mutate(Presence = 1) %>%
    dplyr::right_join(species_project_possible, by = join_cols) %>%
    dplyr::mutate(Presence = dplyr::coalesce(Presence, 0)) %>%
    dplyr::select(dplyr::all_of(c("ProjectShortName",
                                  "SiteID",
                                  "SubStation",
                                  "Iteration",
                                  if(daily) "Date",
                                  "scientific_name",
                                  "common_name",
                                  "Presence"))) %>%
    dplyr::filter(!is.na(common_name) | !is.na(scientific_name))

  if(return_data) {
    return(dplyr::collect(species_occ))
  } else {
    return(dbplyr::remote_query(species_occ))
  }

}
