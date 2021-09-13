#' preview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_preview_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    tags$style(HTML("
                  .col-sm-5 {
                    height:80vh;
                    overflow-y:scroll
                  }
                  ")),    
    
    br(),
    "Name:",
    textOutput(ns("employee_name"), inline = TRUE),
    
    br(),
    "Department:",
    textOutput(ns("department"), inline = TRUE),
    
    br(),
    "FTE:",
    textOutput(ns("fte"), inline = TRUE),
    
    
    br(),
    "Publications:",
    htmlOutput(ns("pub"), inline = FALSE),
    
    br(),
    "Conference:",
    textOutput(ns("conference_foreign")),
    br(),
    textOutput(ns("conference_local")),
    

        
        
        
)
  
}
    
#' preview Server Function
#'
#' @noRd 
mod_preview_server <- function(id,
                               identification) {
  moduleServer(id, function(input, output, session) {
    
    # Identification
    if (isTruthy(identification)) {
    output$employee_name <- renderText({identification$employee_name})
    output$department <- renderText({identification$department})
    output$fte <- renderText({identification$fte})
    output$comment <- renderText({identification$comment})
    }
    

    
    output$pub <- renderText({paste(publications(), collapse = "</br>")})
    
    output$conference_foreign <- renderText({conference_foreign()})
    output$conference_local <- renderText({conference_local()})
    
    
    
  }
  
  )
}

    
## To be copied in the UI
# mod_preview_ui("preview_ui_1")
    
## To be copied in the server
# callModule(mod_preview_server, "preview_ui_1")
 
