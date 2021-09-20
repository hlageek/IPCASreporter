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
  
    
   uiOutput(ns("pubs"))
    
  ),
  
  column(width = 8,
         
         
         h2("I. VYDANÉ PUBLIKACE"),
         p( "Včetně odkazu do ASEP"),
         
         htmlOutput(ns("section_i"), inline = FALSE),
         
         
         )
  )
}
    
#' pub Server Function
#'
#' @noRd 
mod_pub_server <-  function(id, identification) {
  moduleServer(id, function(input, output, session) {

    

    
    output$pubs <- renderUI({

      if (!isTruthy(identification$employee_name)) {
        
        "Fill your identification details first."
        
      } else {
        
       citations <-  get_asep(identification$employee_name)
       
       if (!is.null(citations)) {
       
       displayed_citations <- purrr::map(
         citations,
         ~stringr::str_replace_all(.x, "<.*?>|\\\\n", " ")
          )
       
       ns <- NS(id)
       
       tagList(

        checkboxGroupInput(ns("publist"), 
                           label ="Most recent publications in ASEP.", 
                           width = "100%",
                           choiceNames = displayed_citations,
                           choiceValues = citations),
        
        actionButton(ns("add"),
                     label = "Update report"
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
    
    section_i <- reactiveValues()
    
    observeEvent(input$add, {
    
      section_i$publist <- input$publist
      
      output$section_i <- renderText({
        paste(section_i$publist, collapse = "<br>")
      })
    
      })
   
    return(section_i)
    
  })
  
}
    

 
