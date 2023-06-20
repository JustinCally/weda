#### Install weda  ####
# Make sure you have devtools installed. Install it if not:
# install.packages("devtools")

# Install weda using devtools:
devtools::install_github("JustinCally/weda")
library(weda)

#### Connect to database ####
# You will need a database password.
# Set it using the keyring package (which is a password manager for Rstudio)

# Make sure you have keyring installed. Install it if not:
# install.packages("keyring")

# Check you have a password for the database and set it if not
if(nrow(keyring::key_list("ari-dev-weda-psql-01")) == 0) {
  keyring::key_set(service = "ari-dev-weda-psql-01", username = "psql_user")
}

# Make sure GoConnect OR Azure VPN is running!!!
con <- weda::weda_connect(password = keyring::key_get(service = "ari-dev-weda-psql-01",
                                                      username = "psql_user"))

#### Launch app ####
# Best viewed in chrome (click open in broser)
weda::camtrap_app(con = con)
