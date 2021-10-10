#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
    

    identification <- mod_identification_server("identification_ui_1")

    section_i <- mod_pub_server("pub_ui_1", identification)
    
    section_iii_undergrad <- mod_undergrad_server("undergrad_ui_1")
    
    section_iii_postgrad <- mod_postgrad_server("postgrad_ui_1")
    
    section_iii_conference <- mod_conference_server( "conference_ui_1")
    
    section_iii_lecture <- mod_lectures_server("lectures_ui_1")
    
    section_iv <- mod_grants_server("grants_ui_1")
    
    mod_preview_server("preview_ui_1", 
                       identification,
                       section_i,
                       section_iii_undergrad,
                       section_iii_postgrad,
                       section_iii_conference,
                       section_iii_lecture,
                       section_iv)
    
    mod_docx_server("docx_ui_1", 
                    identification,
                    section_i,
                    section_iii_undergrad,
                    section_iii_postgrad,
                    section_iii_conference,
                    section_iii_lecture,
                    section_iv)
    
}
