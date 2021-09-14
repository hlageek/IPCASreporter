#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
    

    identification <- mod_identification_server("identification_ui_1")

    publications <- mod_pub_server("pub_ui_1", identification)
    
    # this module needs current values for its output - download
    # therefore current values are passed by parentheses ()
    
    
    callModule(mod_undergrad_server, "undergrad_ui_1", r = r)
    
    callModule(mod_postgrad_server, "postgrad_ui_1", r = r)
    
    conference_foreign <- mod_conference_server( "conference_ui_1")
    
    conference_local <- mod_conference_server( "conference_ui_2")
    
    mod_grants_server( "grants_ui_1" )
    
    mod_av21_server( "av21_ui_1" )
    
    # this module needs to react to changes
    # therefore reactive values are passed *without* parentheses ()
    mod_preview_server("preview_ui_1", 
                       identification)
    
    mod_docx_server("docx_ui_1", 
                    r = r,
                    employee_name = employee_name(),
                    department = department(),
                    publications = publications())
    
}
