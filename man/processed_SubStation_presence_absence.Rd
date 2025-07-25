% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/processed-presence-absence-views.R
\name{processed_SubStation_presence_absence}
\alias{processed_SubStation_presence_absence}
\alias{processed_transect_presence_absence}
\title{Presence/Absence Views}
\usage{
processed_SubStation_presence_absence(
  con,
  return_data = FALSE,
  daily = FALSE,
  species = "all"
)

processed_transect_presence_absence(con, return_data = FALSE, species = "all")
}
\arguments{
\item{con}{database connection}

\item{return_data}{logical flag to return data (TRUE) or sql query (default is FALSE)}

\item{daily}{logical flag to make the query/data be at a daily level (TRUE), or per substation (FALSE)}

\item{species}{whether to make table for 'all' species (default), otherwise accepts a vector of common names (see vba_name_conversions)}
}
\value{
sql or data.frame
}
\description{
presence-absence data views for each site/substation (including a daily presence-absence)
}
\examples{
\dontrun{
con <- weda_connect(password = keyring::key_get(service = "ari-dev-weda-psql-01",
username = "psql_user"))
DBI::dbExecute(conn = con,
               paste(SQL("CREATE VIEW camtrap.processed_site_substation_presence_absence AS"),
               processed_SubStation_presence_absence(con = con, return_data = FALSE)))
}
}
