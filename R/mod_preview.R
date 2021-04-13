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
    htmlOutput(NS(id, "pub"), inline = FALSE)
        
        
)
  
}
    
#' preview Server Function
#'
#' @noRd 
mod_preview_server <- function(input, output, session, r){
  ns <- session$ns

    output$employee_name <- renderText({r$employee_name})
    output$department <- renderText({r$department})
    output$fte <- renderText({r$fte})
    
    pubs <- reactive({
      
      pubs_fun <- function(x) paste(x, '<br/>')
      
      paste(lapply(r$pub, pubs_fun), collapse = "")
      
    })
    
    output$pub <- renderText({pubs()})
   
}
    
## To be copied in the UI
# mod_preview_ui("preview_ui_1")
    
## To be copied in the server
# callModule(mod_preview_server, "preview_ui_1")
 
