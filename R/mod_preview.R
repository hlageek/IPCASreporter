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
    textOutput(NS(id, "employee_name"), inline = TRUE),
    
    br(),
    "Department:",
    textOutput(NS(id, "department"), inline = TRUE),
    
    br(),
    "FTE:",
    textOutput(NS(id, "fte"), inline = TRUE),
    
    
    br(),
    "PUB:",
    htmlOutput(NS(id, "pub"), inline = FALSE),
    
    br(),
    "Conference:",
    textOutput(NS(id, "conference_foreign")),
    br(),
    textOutput(NS(id, "conference_local")),
    

        
        
        
)
  
}
    
#' preview Server Function
#'
#' @noRd 
mod_preview_server <- function(id,
                               r,
                               employee_name,
                               department,
                               conference_foreign,
                               conference_local,
                               publications) {
  
  moduleServer(id, function(input, output, session) {
    
    output$employee_name <- renderText({employee_name()})
    output$department <- renderText({r$department})
    output$fte <- renderText({r$fte})
    

    
    output$pub <- renderText({paste(r$pub, collapse = "")})
    
    output$conference_foreign <- renderText({conference_foreign()})
    output$conference_local <- renderText({conference_local()})
    
    
    
  }
  
  )
}

    
## To be copied in the UI
# mod_preview_ui("preview_ui_1")
    
## To be copied in the server
# callModule(mod_preview_server, "preview_ui_1")
 
