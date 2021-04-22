#' add_remove UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_add_remove_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    actionButton(ns("save"), label = "Add to report"),
    
    actionButton(ns("remove"), label = "Remove from report")
    
    
  )
}

#' add_remove Server Function
#'
#' @noRd 
mod_add_remove_server <- function(id, data, r) {
  moduleServer(id, function(input, output, session) {
    
    
    r$pub <- c()
    
    # Add to report
    
    observeEvent( input$save , {
      r$pub <- c(r$pub, data)
      #cat(file = stderr(), r$pub)
    })
    
    # Remove from report
    
    observeEvent( input$remove , {
      
      if (length(r$pub) < 2) {
        r$pub <- c()
        
      } else {
        
        r$pub <- r$pub[1:(length(r$pub)-1)]
        
      }
      
      
    })
    

    
      
  })
}

## To be copied in the UI
# mod_add_remove_ui("add_remove_ui_1")

## To be copied in the server
# callModule(mod_add_remove_server, "add_remove_ui_1")

