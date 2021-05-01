#' int_projects UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_int_projects_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    textInput(ns("int_projects_name"), label = "Název projektu"),

    mod_add_remove_ui(ns("add_remove_ui_1"))
    
 
  )
}
    
#' int_projects Server Function
#'
#' @noRd 
mod_int_projects_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
  })}

    
## To be copied in the UI
# mod_int_projects_ui("int_projects_ui_1")
    
## To be copied in the server
# mod_int_projects_server("int_projects_ui_1")
 
