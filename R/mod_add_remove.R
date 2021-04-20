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
mod_add_remove_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    
    result <- reactiveValues()

    # Add to report

    observeEvent( input$save , {
      result$vector <- c(result$vector(), data)

          cat(file = stderr(), result$vector)
      
    })

    # Remove from report

    observeEvent( input$remove , {

      if (length(result$vector()) < 2) {
        result$vector <- c()

      } else {

        result$vector <- result$vector()[1:(length(result$vector())-1)]
        cat(file = stderr(), result$vector)
        

      }
    }
    )
    

  
      reactive({
        test <- result()$vector
        
        })
      
  })
}

## To be copied in the UI
# mod_add_remove_ui("add_remove_ui_1")

## To be copied in the server
# callModule(mod_add_remove_server, "add_remove_ui_1")

