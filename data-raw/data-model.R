library(dplyr)
library(dbplyr)
library(weda)
library(ReDaMoR)

# schema <- "camtrap"
#
# tables <- data_dictionary %>%
#                    filter(schema == schema) %>%
#                    pull(table_name) %>%
#   unique()
#
# all_tables <- sapply(tables, function(x) {
#
#   fdf <- data_dictionary %>%
#     filter(schema == schema & table_name == x) %>%
#     mutate(nullable = FALSE) %>%
#     select(name = column_name,
#            type = column_class,
#            nullable,
#            comment = column_description)
#
#   mod <- RelTableModel(tableName = x, fields = fdf)
#
#   return(mod)
#
# }, simplify = FALSE)

# Make connection
con_odbc <- RPostgreSQL::dbConnect(odbc::odbc(),
                                   Driver = "PostgreSQL Driver",
                                   Server = "10.110.7.201",
                                   Database = "ari-dev-weda-psql-01",
                                   UID = "psql_user",
                                   PWD = keyring::key_get(service = "ari-dev-weda-psql-01",
                                                          username = "psql_user"),
                                   Port = 5432,
                                   sslmode = "require",
                                   maxvarcharsize = 0)

## Select schema
schema <- "camtrap"

## Get tables
tables <- dbGetQuery(con_odbc,
                     paste0("SELECT table_name FROM information_schema.tables
                   WHERE table_schema='",schema,"'"))

all_tables <- sapply(tables[,1], function(x) {

  tab <- tbl(con_odbc, in_schema(schema = schema, table = x)) %>%
    head() %>%
    collect() %>%
    mutate(across(where(hms::is_hms),as.character))

  return(tab)
}, simplify = F)


m <- df_to_model(list = names(all_tables), envir = as.environment(all_tables))

# m %>%
#   auto_layout(lengthMultiplier=250) %>%
#   plot()

m <- model_relational_data(m)

write_json_data_model(m, "inst/data-models/camtrap-model.json")

m2 <- read_json_data_model("inst/data-models/camtrap-model.json")
