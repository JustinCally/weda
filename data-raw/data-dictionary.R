# libraries
library(dplyr)
library(dbplyr)

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
schema <- "test"

## Get tables
tables <- dbGetQuery(con_odbc,
           paste0("SELECT table_name FROM information_schema.tables
                   WHERE table_schema='",schema,"'"))

data_dict <- sapply(tables[,1], function(x) {

  tab <- tbl(con_odbc, in_schema(schema = schema, table = x)) %>%
                head() %>%
                collect()

df <- data.frame(schema = schema,
             table_name = x,
             table_description = NA_character_,
             table_type = stringr::str_extract(x, "[^_]+"),
             column_name = names(tab),
             column_class = c(sapply(tab,
                                     function(y) paste(class(y), collapse = ", "),
                                     simplify = TRUE)),
             column_description = NA_character_,
             table_links = NA_character_)

return(df)
}, simplify = F) %>% bind_rows() %>% `row.names<-`(NULL)
