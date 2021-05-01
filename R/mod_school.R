#' school UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_school_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    textInput(ns("school_title"), label = "Název přednášky či specifikace jiného druhu akce"),
    textInput(ns("school_name"), label = "Pořadatel/škola"),
    textAreaInput(ns("school_description"), label = "Popis činnosti" ),
    
    mod_add_remove_ui(ns("add_remove_ui_1"))
    
    
 
  )
}
    
#' school Server Function
#'
#' @noRd 
mod_school_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
  })}
    
## To be copied in the UI
# mod_school_ui("school_ui_1")
    
## To be copied in the server
# mod_school_server("school_ui_1")
 
