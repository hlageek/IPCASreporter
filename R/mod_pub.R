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
    
    textInput(NS(id, "asep_code"), label = "ASEP item code", value = "0467096"),
    
    uiOutput(NS(id, "title")),
    
    checkboxInput(NS(id, "significant"), label = "A significant output."),
    
    uiOutput(NS(id, "additional_info"))
    
  )
}
    
#' pub Server Function
#'
#' @noRd 
mod_pub_server <- function(input, output, session, r){
  ns <- session$ns
  

  
  observeEvent( input$asep_code  , {
    
    
    r$pubs <- get_asep(input$asep_code)
      
  })
  
  output$title <- renderText({ 
    
    if (isTruthy(input$asep_code)) {

      HTML(get_asep(input$asep_code))
  
  } else {
    
     "Insert ASEP code."
    
  }
    
    })
  
  title2 <- renderText({ 
    
    if (isTruthy(input$asep_code)) {
      
      HTML(get_asep(input$asep_code))
      
    } else {
      
      "Insert ASEP code."
      
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
      
      textAreaInput("citation", label = "Bibliographical citation", value = title2() , width = '80%')

      
      
      
      
      
      
      
      
      )
      
    }
    
    
  })
 
}
    
## To be copied in the UI
# mod_pub_ui("pub_ui_1")
    
## To be copied in the server
# callModule(mod_pub_server, "pub_ui_1")
 
