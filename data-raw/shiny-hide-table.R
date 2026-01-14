# create table to hide recs from shiny
library(dbplyr)
library(weda)

shiny_hide_table <- data.frame(ProjectShortName = "budj_bim_pig_deer_2022",
                               Iteration = 2)

con <- weda_connect(password = keyring::key_get(service = "ari-dev-weda-psql-01",
                                                username = "psql_user"))


DBI::dbWriteTable(con, DBI::Id(schema = "camtrap", table = "shiny_hide_table"), shiny_hide_table)
