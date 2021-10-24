#' events UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_events_ui <- function(id){
  ns <- NS(id)
  
  fluidRow(column(width = 4,
                  
                  
                  uiOutput(ns("events"))
                  
  ),
  
  column(width = 8,
         
         
         htmlOutput(ns("section_ii"), inline = FALSE),
         
         
  )
  )
  
}
    
#' events Server Function
#'
#' @noRd 
mod_events_server <- function(id, identification) {
  moduleServer(id, function(input, output, session) { 
    
    
    output$events <- renderUI({
      
      if (!isTruthy(identification$employee_name)) {
        
        "Fill your identification details first."
        
      } else {
        
        citations <-  get_asep(identification$employee_name, type = "events")
        
        if (!is.null(citations)) {
          
          displayed_citations <- purrr::map(
            citations,
            ~stringr::str_replace_all(.x, "<.*?>", " ")
          )
          
          ns <- NS(id)
          
          tagList(
            
            checkboxGroupInput(ns("eventlist"), 
                               label ="Most recent events in ASEP.", 
                               width = "100%",
                               choiceNames = displayed_citations,
                               choiceValues = citations),
            
            actionButton(ns("add"),
                         label = "Add to report"
            )
          )
        } else {
          
          paste0("No ASEP records found for author ", 
                 identification$employee_name, 
                 " in year ", 
                 format(Sys.Date(), "%Y"), 
                 " or ", 
                 format(Sys.Date()-365, "%Y"), 
                 ".")
        }
        
      }
      
      
    })
    
    section_ii <- reactiveValues()
    
    observeEvent(input$add, {
      
      section_ii$eventlist <- input$eventlist
      
      output$section_ii <- renderText({
        paste(section_ii$eventlist, collapse = "<br>")
      })
      
    })
    
    return(section_ii)
    
  })
    }
    