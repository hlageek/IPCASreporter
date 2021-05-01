#' popular UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_popular_ui <- function(id){
  ns <- NS(id)
  tagList(
    

    textInput(ns("popular_contribution"), label = "Název akce"),
    textInput(ns("popular_description"), label = "Popis aktivity"),
    textInput(ns("popular_organizer_primary"), label = "Hlavní pořadatel"),
    textInput(ns("popular_organizer_secondary"), label = "Spolupořadatel"),
    textInput(ns("popular_place"), label = "Místo konání akce"),
    dateInput(ns("popular_date"), label = "Datum konání akce"),
    
    mod_add_remove_ui(ns("add_remove_ui_1"))
    
    
  )
}
    
#' popular Server Function
#'
#' @noRd 
mod_popular_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
   
  })}
    
## To be copied in the UI
# mod_popular_ui("popular_ui_1")
    
## To be copied in the server
# callModule(mod_popular_server, "popular_ui_1")
 
