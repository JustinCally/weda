#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(weda)
readRenviron(".Renviron")
con <- odbc::dbConnect(odbc::odbc(), Driver = "PostgreSQL",
                              Server = "10.110.7.201",
                              Database = "ari-dev-weda-psql-01",
                              UID = "psql_user",
                              PWD = Sys.getenv("psql_pwd"),
                              Port = 5432, BoolsAsChar = "No",
                              maxvarcharsize = 0, timeout = 30)

# con <- weda::weda_connect(password = Sys.getenv("psql_pwd"))
weda::camtrap_app(con = con)

# Run the application
# shinyApp(ui = ui, server = server)
