#' manager UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_manager_ui <- function(id, i18n){
  ns <- NS(id)

      tabsetPanel(
          tabPanel("Osobní náhled",
                   preview_standard(ns, i18n)
                   ),
          tabPanel("Náhled oddělení")
      )
  
}
    
#' manager Server Functions
#'
#' @noRd 
mod_manager_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_manager_ui("manager_1")
    
## To be copied in the server
# mod_manager_server("manager_1")
