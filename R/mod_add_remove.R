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
    
    actionButton(ns("save"), label = i18n$t("Zadat do výkazu") ,                  icon = icon("check"),                  class = "btn-success"),
    
    actionButton(ns("remove"), label = i18n$t("Odstranit z výkazu") )
    
    
  )
}

#' add_remove Server Function
#'
#' @noRd 
mod_add_remove_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    reactive({
      data <- data()
    })
    
    input_value <- reactiveVal(c())
    
    # Zadat do výkazu
    
    observeEvent( input$save , {
      input_value <- input_value(c(input_value(), data()))
# cat(file = stderr(), input_value())

    })
    
    # Remove 
    
    observeEvent( input$remove , {
      
      if (length(input_value()) < 2) {

        input_value <- input_value(c())


      } else {
        
        input_value <- input_value(input_value()[1:(length(input_value())-1)])
        
      }
      
      
    })

    return(input_value)
    
    
      
  })
}

## To be copied in the UI
# mod_add_remove_ui("add_remove_ui_1")

## To be copied in the server
# callModule(mod_add_remove_server, "add_remove_ui_1")

