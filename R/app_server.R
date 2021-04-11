#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here

    callModule(mod_employee_name_server, "employee_name_ui_1")
    
    callModule(mod_department_server, "department_ui_1")
}
