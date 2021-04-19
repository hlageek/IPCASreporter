#' conference UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_conference_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    textInput(ns("conference_contribution"), label = "Contribution title"),
    textInput(ns("conference_organizer"), label = "Organizer"),
    textInput(ns("conference_name"), label = "Název konference"),
    dateInput(ns("conference_date"), label = "Datum konání"),
    make_add_remove_ui(ns)

  )
  
}
    
#' conference Server Function
#'
#' @noRd 
mod_conference_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    reactive(
      paste(input$conference_contribution, 
            input$conference_organizer,
            input$conference_name,
            input$conference_date)
    ) 
    
    
  }
  )
}
## To be copied in the UI
# mod_conference_ui("conference_ui_1")
    
## To be copied in the server
# callModule(mod_conference_server, "conference_ui_1")
 
