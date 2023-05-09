# Run app function
weda::camtrap_app(con = weda::weda_connect(password = keyring::key_get(service = "ari-dev-weda-psql-01",
                                                                 username = "psql_user")))
