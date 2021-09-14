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
  tagList(
    
   uiOutput(ns("pubs"))
    
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
    
    publications <- reactiveValues()
    
    observeEvent(req(input$add), {

    publications <- reactiveValues(input$publist)
    print(publications())
    })
   
    return(publications)
    
  })
  
}
    

 
