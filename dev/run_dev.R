ipcas_db <- pool::dbPool(
    drv = RMariaDB::MariaDB(),
    dbname = "ipcas",
    username = "test",
    password = "test"
)
onStop(function() {
    pool::poolClose(ipcas_db)
})

# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Run the application
(run_app())
