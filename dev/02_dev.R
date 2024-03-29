# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package( "tibble" )
usethis::use_package( "magrittr" )
usethis::use_package( "rvest" )
usethis::use_package( "stringr" )
usethis::use_package( "officer" )
usethis::use_package( "here" )
usethis::use_package( "dplyr")
usethis::use_package( "dbplyr")
usethis::use_package( "keyring")
usethis::use_package( "mailR")
usethis::use_package( "shinythemes")
usethis::use_package( "golem")
usethis::use_package( "pool")
usethis::use_package( "DBI")
usethis::use_package( "RMariaDB")
usethis::use_package( "shinymanager")
usethis::use_package( "purrr")
usethis::use_package( "readr")
usethis::use_package( "tidyr")
usethis::use_package( "shiny.i18n")
usethis::use_package( "waiter")












## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "identification" ) # Name of the module
golem::add_module( name = "pub" ) # Name of the module
golem::add_module( name = "docx" ) # Name of the module
golem::add_module( name = "preview" ) # Name of the module
golem::add_module( name = "undergrad" ) # Name of the module
golem::add_module( name = "postgrad" ) # Name of the module
golem::add_module( name = "conference" ) # Name of the module
golem::add_module( name = "events" ) # Name of the module
golem::add_module( name = "lectures" ) # Name of the module
golem::add_module( name = "grants" ) # Name of the module
golem::add_module( name = "av21" ) # Name of the module
golem::add_module( name = "popular" ) # Name of the module
golem::add_module( name = "school" ) # Name of the module
golem::add_module( name = "public" ) # Name of the module
golem::add_module( name = "media" ) # Name of the module
golem::add_module( name = "int_projects" ) # Name of the module
golem::add_module( name = "int_bilateral" ) # Name of the module
golem::add_module( name = "other_award" ) # Name of the module
golem::add_module( name = "other_review" ) # Name of the module
golem::add_module( name = "other_member" ) # Name of the module
golem::add_module( name = "other_editions" ) # Name of the module
golem::add_module( name = "wip" ) # Name of the module
golem::add_module( name = "various" ) # Name of the module
golem::add_module( name = "guide" ) # Name of the module
golem::add_module( name = "manager" ) # Name of the module







## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct("test_modules") # modules as apps
golem::add_fct("compile_docx")
golem::add_utils( "helpers" )
golem::add_utils( "funs", module = "pub" )
golem::add_utils( "funs", module = "undergrad" )
golem::add_utils( "funs", module = "postgrad" )
golem::add_utils("db_startup")
golem::add_utils( "funs", module = "manager" )





## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "universities", open = FALSE ) 
usethis::use_data_raw(name = "departments", open = FALSE ) 
usethis::use_data_raw(name = "providers", open = FALSE)

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("IPCASreporter")
devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

