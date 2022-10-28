#' pub UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_pub_ui <- function(id){
  ns <- NS(id)
  
  fluidRow(column(width = 4,
  
    
   uiOutput(ns("pubs")),
   
   
   actionButton(ns("add"),
                label = "Add to report",                  icon = icon("check"),                  class = "btn-success"
   )
    
  ),
  
  column(width = 8,
         
         
         htmlOutput(ns("section_i"), inline = FALSE),
         
         
         )
  )
}
    
#' pub Server Function
#'
#' @noRd 
mod_pub_server <-  function(id, identification) {
  moduleServer(id, function(input, output, session) {

      ns <- NS(id)
      

    
    output$pubs <- renderUI({

      if (!isTruthy(identification$employee_name)) {
        
        "Fill your identification details first."
        
      } else {
        
       citations <-  get_asep(identification$employee_name, type = "pubs")
       
       if (!is.null(citations)) {
       
       displayed_citations <- purrr::map(
         citations,
         ~stringr::str_replace_all(.x, "<.*?>", " ")
          )
       
       
       tagList(

        checkboxGroupInput(ns("publist"), 
                           label ="Most recent publications in ASEP.", 
                           width = "100%",
                           choiceNames = displayed_citations,
                           choiceValues = citations)

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
    
    section_i <- reactiveValues()
    
    observeEvent(input$add, {
    
        browser()
      section_i$publist <- input$publist
      
      output$section_i <- renderText({
        paste(section_i$publist, collapse = "<br>")
      })
    
      })
   
    
    return(section_i)
    
  })
  
}
    

 
