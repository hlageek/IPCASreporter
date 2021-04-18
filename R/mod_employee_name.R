
#' employee_name UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_employee_name_ui <- function(id){
  ns <- NS(id)
  tagList(
    textInput(ns("employee_name"), "Full name", value = "")
    
  )
}
    
#' employee_name Server Function
#'
#' @noRd 
mod_employee_name_server <- function(id) {
  moduleServer(id, function(input, output, session) {
  
  
   reactive(input$employee_name)
  


})
}
    
## To be copied in the UI
# mod_employee_name_ui("employee_name_ui_1")
    
## To be copied in the server
# callModule(mod_employee_name_server, "employee_name_ui_1")
 
