#' lectures UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_lectures_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    textInput(ns("lecture_contribution"), label = "Lecture title"),
    textInput(ns("lecture_organizer"), label = "Organizer"),
    textInput(ns("lecture_name"), label = "Název akce"),
    dateInput(ns("lecture_date"), label = "Datum konání"),
    radioButtons(ns("lecture_location"), label = "Kategorie", choices = c("Domácí", "Zahraniční"), inline = TRUE),
    mod_add_remove_ui(ns("add_remove_ui_1"))
    
    
  )
}
    
#' lectures Server Function
#'
#' @noRd 
mod_lectures_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    
    
  })}
 

    
## To be copied in the UI
# mod_lectures_ui("lectures_ui_1")
    
## To be copied in the server
# callModule(mod_lectures_server, "lectures_ui_1")
 
