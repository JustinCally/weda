% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shiny-app.R
\name{camtrap_app}
\alias{camtrap_app}
\alias{transect_app}
\title{Run the camera trap app}
\usage{
camtrap_app(con)

transect_app(con)
}
\arguments{
\item{con}{database connection}
}
\value{
shiny app
}
\description{
An app that allows users to view camera trap coverage survey coverage,
download data and upload data
}
\examples{
\dontrun{
con <- weda_connect(password = keyring::key_get(service = "ari-dev-weda-psql-01",
username = "psql_user"))
camtrap_app(con = con)
}
}
