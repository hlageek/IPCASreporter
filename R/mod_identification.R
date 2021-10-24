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
  
  fluidRow(column(width = 4,

    textInput(ns("employee_name"), 
              "Jméno a příjmení", 
              value = "",
              placeholder = "Eva Zažímalová"
              ),
    
    selectInput(ns("department"),
                label = "Oddělení", 
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
                  label = "Komentář",
                  placeholder = "Changes in FTE during the year or similar."
                  ),
    
    actionButton(ns("add"),
                 label = "Add to report"
                 )
 
  
  ),
  
  column(width = 8,
         
         br(),
         "Jméno:",
         textOutput(ns("employee_name"), inline = TRUE),
         
         br(),
         "Oddělení:",
         textOutput(ns("department"), inline = TRUE),
         
         br(),
         "FTE:",
         textOutput(ns("fte"), inline = TRUE),
         
         br(),
         "Komentář:",
         textOutput(ns("comment"), inline = TRUE)
         
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
     
      identification$employee_name <- input$employee_name
      identification$department <- input$department
      identification$fte <- input$fte
      identification$comment <- input$comment
      
      output$employee_name <- renderText({identification$employee_name})
      output$department <- renderText({identification$department})
      output$fte <- renderText({identification$fte})
      output$comment <- renderText({identification$comment})
        
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

    


