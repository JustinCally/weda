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

  RPostgreSQL::dbConnect(RPostgres::Postgres(),
                                host = '10.110.7.8',
                                dbname = 'ari-dev-weda-psql-01',
                                user = username,
                                password = password,
                                port = 5432,
                                service = NULL,
                                list(sslmode = "require"),
                         ...)
}
