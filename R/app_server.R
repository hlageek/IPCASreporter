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
    
    callModule(mod_department_server, "department_ui_1", r = r)
    
    callModule(mod_fte_server, "fte_ui_1", r = r)
    
    callModule(mod_pub_server, "pub_ui_1", r = r)
    
    callModule(mod_docx_server, "docx_ui_1", r = r)
    
    
    callModule(mod_undergrad_server, "undergrad_ui_1", r = r)
    
    callModule(mod_postgrad_server, "postgrad_ui_1", r = r)
    
    conference_foreign <- mod_conference_server( "conference_ui_1")
    
    conference_local <- mod_conference_server( "conference_ui_2")
    

    
    mod_preview_server("preview_ui_1", 
                       r = r, 
                       employee_name = employee_name,
                       conference_foreign = conference_foreign,
                       conference_local = conference_local)
}
