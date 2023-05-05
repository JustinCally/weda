#' Database Connection
#'
#' @description function to easily connect to the weda database
#'
#' @param username postgres user name (default is psql_user)
#' @param password postgres password (keyring use suggested)
#' @param ... additional arguments passed to \code{RPostgreSQL::dbConnect()}
#'
#' @return PostgreSQL connection
#' @export
#'
#' @examples
#' \dontrun{
#'
#' weda_connect(password = keyring::key_get(service = "ari-dev-weda-psql-01", username = "psql_user"))
#'
#' }
weda_connect <- function(username = "psql_user",
                         password,
                         ...) {

  RPostgreSQL::dbConnect(odbc::odbc(),
                         Driver = "PostgreSQL Driver",
                         Server = '10.110.7.201',
                         Database = 'ari-dev-weda-psql-01',
                         UID = username,
                         PWD = password,
                         Port = 5432,
                         BoolsAsChar = 'No',
                         sslmode = 'require',
                         maxvarcharsize = 0,
                         ...)
}
