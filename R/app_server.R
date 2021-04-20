#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
    
    library(magrittr)
  
    r <- reactiveValues()

    employee_name <- mod_employee_name_server("employee_name_ui_1")
    
    department <- mod_department_server("department_ui_1", r= r)
    
    callModule(mod_fte_server, "fte_ui_1", r = r)
    
    publications <- mod_pub_server("pub_ui_1", r = r)
    
    # this module needs current values for its output - download
    # therefore current values are passed by parentheses ()
    mod_docx_server("docx_ui_1", 
               r = r,
               employee_name = employee_name(),
               department = department())
    
    
    callModule(mod_undergrad_server, "undergrad_ui_1", r = r)
    
    callModule(mod_postgrad_server, "postgrad_ui_1", r = r)
    
    conference_foreign <- mod_conference_server( "conference_ui_1")
    
    conference_local <- mod_conference_server( "conference_ui_2")
    
    # this module needs to react to changes
    # therefore reactive values are passed *without* parentheses ()
    mod_preview_server("preview_ui_1", 
                       r = r, 
                       employee_name = employee_name,
                       department = department,
                       conference_foreign = conference_foreign,
                       conference_local = conference_local,
                       publications = publications)
    
}
