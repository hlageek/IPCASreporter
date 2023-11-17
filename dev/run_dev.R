# pool <- pool::dbPool(
#     drv = RMariaDB::MariaDB(),
#     dbname = "ipcas",
#     username = "test",
#     password = "test"
# )

# Set options here
options(golem.app.prod = TRUE) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Run the application
(run_app(   email_password = keyring::key_get(service = "flumail",
                                                username = "flu.avcr"),
            email_default = Sys.getenv("golem.email"),
            dbname = "ipcas",
            dbusername = "test",
            dbpassword = "test",
            credentials_path = "credentials.sqlite",
            credentials_pass = "test",
            deadline = "2023-11-17 15:00:00"
            )
    )
