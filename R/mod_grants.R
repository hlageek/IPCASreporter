#' grants UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_grants_ui <- function(id){
  ns <- NS(id)
  
  tagList(
 
    textInput(ns("grant_number"), label = "Project number"),
    textInput(ns("grant_title"), label = "Title"),
    selectInput(ns("grant_provider"),
                label = "Provider",
                selected = "",
                choices = 
                c("",
                  "GAČR",
                  "TAČR",
                  "MŠMT")),
    selectInput(ns("grant_date_from"), label = "Doba řešení od",
                selected = "",
                choices = c("",
                            seq(2010, 2030, 1))),
    selectInput(ns("grant_date_to"), label = "Doba řešení do",
                selected = "",
                choices = c("",
                            seq(2010, 2030, 1))),
    conditionalPanel(
      condition = "input.grant_date_from == output.current_year",
      
      textAreaInput(ns("annotation_cze"), label = "Anotace česky"),
      textAreaInput(ns("annotation_eng"), label = "Anotace anglicky"),
      
      ns = NS(id)),
    mod_add_remove_ui(ns("add_remove_ui_1"))
  )
}
    
#' grants Server Function
#'
#' @noRd 
mod_grants_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
 output$current_year <- renderText({format(Sys.time(), "%Y")})
 outputOptions(output, "current_year", suspendWhenHidden = FALSE)
 
        
        
  
    
    
  })}
    
## To be copied in the UI
# mod_grants_ui("grants_ui_1")
    
## To be copied in the server
# callModule(mod_grants_server, "grants_ui_1")
 
