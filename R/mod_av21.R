#' av21 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_av21_ui <- function(id){
  ns <- NS(id)
  tagList(
    
      textInput(ns("program"), label = "Program Strategie AV21"),
      textInput(ns("activity"), label = "Název aktivity (projektu)"),
      textInput(ns("person"), label = "Řešitel aktivity (projektu)"),
      textAreaInput(ns("annotation_cze"), label = "Anotace česky",
                    placeholder = "Lze zkopírovat z návrhového listu aktivity"),
      textAreaInput(ns("annotation_eng"), label = "Anotace anglicky"),
      textAreaInput(ns("results"), label = "Výstupy (včetně příp. odkazu na ASEP)"),
      div(style="display: inline-block;vertical-align:baseline;", 
          textInput(ns("partner"), label = "Spolupracující instituce")),
      div(style="display: inline-block;vertical-align:baseline;",
          actionButton(ns("add_partner"), label = "+")),
      uiOutput(ns("partner_plus")),

      mod_add_remove_ui(ns("add_remove_ui_1"))
    
 
  )
}
    
#' av21 Server Function
#'
#' @noRd 
mod_av21_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent( input$add_partner , {
      
      output$partner_plus <- renderUI({
        
        tagList(
          
          textInput(paste0(id, input$add_partner), label = "Spolupracující instituce")
        
        )
   
        
        })
      })
    
    
    
  })}
 

    
## To be copied in the UI
# mod_av21_ui("av21_ui_1")
    
## To be copied in the server
# callModule(mod_av21_server, "av21_ui_1")
 
