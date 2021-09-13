#' identification UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_identification_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    textInput(ns("employee_name"), 
              "Full name", 
              value = ""
              ),
    
    selectInput(ns("department"),
                label = "Department", 
                selected = "", 
                choices = c("", departments$department_name)
                ),
    
    sliderInput(ns("fte"), 
                label = "FTE", 
                value = 0, 
                min = 0, 
                max = 1, 
                step = 0.01
                ),
    
    textAreaInput(ns("comment"), 
                  label = "Comment",
                  placeholder = "Changes in FTE during the year or similar."
                  ),
    
    actionButton(ns("add"),
                 label = "Add to report"
                 )
 
  )
}
    
#' identification Server Function
#'
#' @noRd 
mod_identification_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    identification <- reactiveValues()
    
    observeEvent(input$add, {
     
      identification$employee_name = input$employee_name
      identification$department = input$department
      identification$fte = input$fte
      identification$comment = input$comment
        
    })
    
    
    return(identification)
    
    
  })
}



identificationApp <- function() {
  ui <- fluidPage(
    mod_identification_ui("identification_ui_1")
  )
  server <- function(input, output, session) {
    
    mod_identification_server("identification_ui_1")
  }
  shinyApp(ui, server)  
}

    


