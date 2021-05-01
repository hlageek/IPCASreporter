#' events UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_events_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    div(style="display: inline-block;vertical-align:baseline;", 
        textInput(ns("asep_code"), label = "Insert ASEP item code", value = "0467096")),
    
    div(style="display: inline-block;vertical-align:baseline;",
        actionButton(ns("asep_search"), label = "Search")),
    
    br(),
    
    mod_add_remove_ui(ns("add_remove_ui_1"))
    
 
  )
}
    
#' events Server Function
#'
#' @noRd 
mod_events_server <- function(id) {
  moduleServer(id, function(input, output, session) { 
    
    
    
    
  })
    }
    
## To be copied in the UI
# mod_events_ui("events_ui_1")
    
## To be copied in the server
# callModule(mod_events_server, "events_ui_1")
 