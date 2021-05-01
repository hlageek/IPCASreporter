#' wip UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_wip_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    textAreaInput(ns("wip_description"), label = "Popis"),
    
    
    mod_add_remove_ui(ns("add_remove_ui_1"))
 
  )
}
    
#' wip Server Function
#'
#' @noRd 
mod_wip_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
  })}
    
## To be copied in the UI
# mod_wip_ui("wip_ui_1")
    
## To be copied in the server
# mod_wip_server("wip_ui_1")
 
