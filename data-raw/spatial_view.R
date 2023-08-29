library(weda)
library(dbplyr)
library(sf)

con <- weda::weda_connect(password = keyring::key_get(service = "ari-dev-weda-psql-01",
                                                      username = "psql_user"))

geom_table <- tbl(con, in_schema("camtrap", "curated_camtrap_operation")) %>%
  mutate(geometry = sql('ST_SetSRID(ST_MakePoint("Longitude", "Latitude"), 4283)')) %>%
  select(camtrap_operation_database_ID, everything())

DBI::dbExecute(conn = con, paste(DBI::SQL("CREATE VIEW camtrap.spatial_camtrap_operation AS"), dbplyr::remote_query(geom_table)))

st_op <- st_read(dsn = con,
                 layer = Id(schema = "camtrap", table = "spatial_camtrap_operation"))

