#' various UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_various_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    textAreaInput(ns("various_description"), label = "Popis"),
    
    
    mod_add_remove_ui(ns("add_remove_ui_1"))
 
  )
}
    
#' various Server Function
#'
#' @noRd 
mod_various_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
  })}
    
## To be copied in the UI
# mod_various_ui("various_ui_1")
    
## To be copied in the server
# mod_various_server("various_ui_1")
 
