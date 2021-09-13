#' pub UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_pub_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    div(style="display: inline-block;vertical-align:baseline;",
        
        textInput(ns("asep_code"), 
                  label = "Insert ASEP item code", 
                  value = "0467096")),
    
    div(style="display: inline-block;vertical-align:baseline;",
        
        actionButton(ns("asep_search"), 
                     label = "Search")),
    
    br(),
    
    uiOutput(ns("title")),
    
    checkboxInput(ns("significant"), label = "A significant output."),
    
    uiOutput(ns("additional_info")),
    
    mod_add_remove_ui(ns("add_remove_ui_1"))
    
  )
}
    
#' pub Server Function
#'
#' @noRd 
mod_pub_server <-  function(id) {
  moduleServer(id, function(input, output, session) {

 
  
title_source <- eventReactive(input$asep_search,{ 
    
    if (isTruthy(input$asep_code)) {
      
      HTML(get_asep(input$asep_code))
      
    } 
 
  })

title <- renderText({ title_source() })
  
  
output$title <- renderText({ 
  if (isTruthy(title_source())) {
  title_source() } else {
    "Enter ASEP item code."
  }
  
  })
    

  output$additional_info <- renderUI({
    

    if (isTruthy(input$significant)) {
      
      tagList(
      textAreaInput("annotation_cze", label = "Annotation in Czech", width = '80%'),
      p("Max. 500 characters."),
      
      textAreaInput("annotation_eng", label = "Annotation in English", width = '80%'),
      p("Max. 500 characters."),
      
      textInput("collaborator", label = "Collaborating organization(s)"),
      
      textInput("author_name", label = "Contact person name"),
      textInput("author_name", label = "Contact person email"),
      textInput("author_name", label = "Contact person phone number"),
      
      textAreaInput("citation", label = "Bibliographical citation", value = title() , width = '80%')

      
      )
      
    }

  })
  
# create output container
  publications <- mod_add_remove_server("add_remove_ui_1", title)
  
  return(publications)
  
 
  })
  
}
    
## To be copied in the UI
# mod_pub_ui("pub_ui_1")
    
## To be copied in the server
# callModule(mod_pub_server, "pub_ui_1")
 
