#' int_bilateral UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_int_bilateral_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    textAreaInput(ns("int_bilateral_description"), label = "Popis spolupráce"),
    
    mod_add_remove_ui(ns("add_remove_ui_1"))
 
  )
}
    
#' int_bilateral Server Function
#'
#' @noRd 
mod_int_bilateral_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
  })}

    
## To be copied in the UI
# mod_int_bilateral_ui("int_bilateral_ui_1")
    
## To be copied in the server
# mod_int_bilateral_server("int_bilateral_ui_1")
 
