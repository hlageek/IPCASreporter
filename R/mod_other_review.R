#' other_review UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_other_review_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    textInput(ns("other_review_name"), label = "Název"),
    textAreaInput(ns("other_review_description"), label = "Doplňující informace"),
    
    
    mod_add_remove_ui(ns("add_remove_ui_1"))
    
 
  )
}
    
#' other_review Server Function
#'
#' @noRd 
mod_other_review_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
  })}
## To be copied in the UI
# mod_other_review_ui("other_review_ui_1")
    
## To be copied in the server
# mod_other_review_server("other_review_ui_1")
 
