# Run app function
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
# ui < function()
# server <- function()
# shinyApp(ui, server)
weda::camtrap_app(con = weda::weda_connect(password = keyring::key_get(service = "ari-dev-weda-psql-01", username = "psql_user")))
