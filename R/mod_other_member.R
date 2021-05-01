#' other_member UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_other_member_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    textInput(ns("other_member_name"), label = "Název"),
    radioButtons(ns("other_member_category"), 
                 label = NULL,
                 choices = c("Domácí",
                             "Zahraniční")),
    
    mod_add_remove_ui(ns("add_remove_ui_1"))
    
 
  )
}
    
#' other_member Server Function
#'
#' @noRd 
mod_other_member_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
  })}
    
## To be copied in the UI
# mod_other_member_ui("other_member_ui_1")
    
## To be copied in the server
# mod_other_member_server("other_member_ui_1")
 
