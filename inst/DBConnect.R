#### Setup ####
library(DBI)
library(dplyr)
library(dbplyr)
library(sf)
library(rpostgis)
library(RPostgres)
library(odbc)
library(raster)

# Set up keyring if not already set. Password for psql_user will be remembered in Rstudio keychain once you set it
if(nrow(keyring::key_list("ari-dev-weda-psql-01")) == 0) {
keyring::key_set(service = "ari-dev-weda-psql-01", username = "psql_user")
}

# Check odbc driver exists
if(!("PostgreSQL Driver" %in% odbc::odbcListDrivers()$name)) {
  stop("Install the postgres driver to connect to database: https://odbc.postgresql.org/")
}

# Note that the DB is currently configured to turn on at 7am and turn off at 7pm to reduce costs
# ODBC Connection : more interactive in rstudio but requires ODBC driver
con_odbc <- RPostgreSQL::dbConnect(odbc::odbc(),
                              Driver = "PostgreSQL Driver",
                              Server = '10.110.7.201',
                              Database = 'ari-dev-weda-psql-01',
                              UID = "psql_user",
                              PWD = keyring::key_get(service = "ari-dev-weda-psql-01", username = "psql_user"),
                              Port = 5432,
                              sslmode = 'require',
                              maxvarcharsize = 0) # issue with binary representation of sf cols

# Postgres connection: better for rasters (and easier to set up)
con <- RPostgreSQL::dbConnect(RPostgres::Postgres(),
                               host = '10.110.7.201',
                               dbname = 'ari-dev-weda-psql-01',
                               user = "psql_user",
                               password = keyring::key_get(service = "ari-dev-weda-psql-01", username = "psql_user"),
                               port = 5432,
                               service = NULL,
                               list(sslmode = "require"))

#### Read and write tables ####
DBI::dbWriteTable(con, Id(schema = "test", table = "mtcars"), mtcars)

# Use lazy evaluation to download a sample of mtcars data
db_mtcars <- tbl(con, in_schema("test", "mtcars")) %>%
  filter(cyl == 6) %>%
  collect()

DBI::dbRemoveTable(con, Id(schema = "test", table = "mtcars"))

#### Spatial datasets (sf) ####
nc <- st_read(system.file("shape/nc.shp", package="sf"))

# Write sf spatial data to db
st_write(obj = nc,
         dsn = con,
         layer = Id(schema = "test", table = "nc"), delete_layer = TRUE)

tbl(con, in_schema("test", "nc"))

# Pull down entire sf layer
nc_db <- st_read(dsn = con,
                 layer = Id(schema = "test", table = "nc"))

# Use postgis to download a part of a layer
nc_wilson_dbplyr <- tbl(con, in_schema("test", "nc")) %>%
  filter(NAME == "Wilson") %>%
  dplyr::select(AREA, NAME, geometry) %>%
  mutate(geometry = ST_ASTEXT(geometry)) %>% #ST_ASTEXT is a postgis function and if dbplyr doesnt know what it is it passes it as such
  collect() %>%
  sf::st_as_sf(wkt = "geometry") # convert to sf

# alternatively you can use st_read with a query
q <- tbl(con, in_schema("test", "nc")) %>%
  filter(NAME == "Wilson") %>%
  dplyr::select(AREA, NAME, geometry) %>%
  dbplyr:::remote_query()

# Pull down entire sf layer
nc_wilson_sf <- st_read(dsn = con, query = q)

DBI::dbRemoveTable(con, Id(schema = "test", table = "nc"))

#### Raster Data ####
# check if the database has PostGIS
pgPostGIS(con)

r <- raster::raster(
  nrows = 180, ncols = 360, xmn = -180, xmx = 180,
  ymn = -90, ymx = 90, vals = 1
)
# Write Raster in the database
pgWriteRast(con, name = c('test','raster'), raster = r, overwrite = TRUE)

#check raster
pgListRast(con)

# get raster
r2 <- rpostgis::pgGetRast(con, name = c('test','raster'), boundary = c(50, 0, 100, 0))

par(mfrow = c(1, 2))
plot(r)
plot(r2)

DBI::dbRemoveTable(con, Id(schema = "test", table = "raster"))
