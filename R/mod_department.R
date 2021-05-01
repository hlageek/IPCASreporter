#' department UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_department_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("department"),
                label = "Department", 
                selected = "", 
                choices = c("", departments$department_name))
  )
}
    
#' department Server Function
#'
#' @noRd 
mod_department_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {

  observeEvent( input$department , {
    r$department <- input$department
  })
  
})
}
    
## To be copied in the UI
# mod_department_ui("department_ui_1")
    
## To be copied in the server
# callModule(mod_department_server, "department_ui_1")
 
