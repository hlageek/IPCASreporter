#' public UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_public_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    textInput(ns("public_body"), label = "Instituce státní nebo veřejné správy"),
    textAreaInput(ns("public_description"), label = "Popis spolupráce"),
    
    mod_add_remove_ui(ns("add_remove_ui_1"))
    
 
  )
}
    
#' public Server Function
#'
#' @noRd 
mod_public_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
  })}
    
## To be copied in the UI
# mod_public_ui("public_ui_1")
    
## To be copied in the server
# mod_public_server("public_ui_1")
 
