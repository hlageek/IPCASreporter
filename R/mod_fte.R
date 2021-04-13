#' fte UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_fte_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    sliderInput(NS(id, "fte"), label = "FTE", value = 0, min = 0, max = 1, step = 0.01)

      )
}
    
#' fte Server Function
#'
#' @noRd 
mod_fte_server <- function(input, output, session, r){
  ns <- session$ns
 
  observeEvent( input$fte , {
    r$fte <- as.character(input$fte[1])
  })
  

  
}
    
## To be copied in the UI
# mod_fte_ui("fte_ui_1")
    
## To be copied in the server
# callModule(mod_fte_server, "fte_ui_1")
 
