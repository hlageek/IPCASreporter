#' other_award UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_other_award_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    textAreaInput(ns("other_award_name"), label = "Název ocenění"),
    
    mod_add_remove_ui(ns("add_remove_ui_1"))
    
 
  )
}
    
#' other_award Server Function
#'
#' @noRd 
mod_other_award_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
  })}
    
## To be copied in the UI
# mod_other_award_ui("other_award_ui_1")
    
## To be copied in the server
# mod_other_award_server("other_award_ui_1")
 
