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
    
    textInput(ns("email"),
                label = "E-mailová adresa", 
                value = "", 
                placeholder = "@flu.cas.cz"
    ),
    
    selectInput(ns("department"),
                label = "Oddělení", 
                selected = "", 
                choices = c("", departments$department_name),
                multiple = TRUE
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
                 label = "Add to report",
                 icon = icon("check"),
                 class = "btn-success"
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
         "E-mail:",
         textOutput(ns("email"), inline = TRUE),
         
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
      identification$department <- paste0(input$department, collapse = "; ")
      identification$fte <- input$fte
      identification$comment <- input$comment
      identification$email <- input$email
      
      output$employee_name <- renderText({identification$employee_name})
      output$department <- renderText({paste0(input$department, collapse = "; ")})
      output$fte <- renderText({identification$fte})
      output$email <- renderText({identification$email})
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

    


