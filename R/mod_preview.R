#' preview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_preview_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    tags$style(HTML("
                  .col-sm-5 {
                    border: 2px solid lightgray;
                    height:100vh;
                    overflow-y:scroll
                  }
                  ")),    
    
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
    textOutput(ns("comment"), inline = TRUE),
    
    
    br(),
    h4("I. VYDANÉ PUBLIKACE"),
    htmlOutput(ns("section_i"), inline = FALSE),
    
    br(),
    h4("III. PEDAGOGICKÁ A PŘEDNÁŠKOVÁ ČINNOST"),
    htmlOutput(ns("section_iii_undergrad"), inline = FALSE),
    htmlOutput(ns("section_iii_postgrad"), inline = FALSE),
    
    
    br(),
    "Conference:",
    textOutput(ns("conference_foreign")),
    br(),
    textOutput(ns("conference_local")),
    

        
        
        
)
  
}
    
#' preview Server Function
#'
#' @noRd 
mod_preview_server <- function(id,
                               identification,
                               section_i,
                               section_iii_undergrad,
                               section_iii_postgrad) {
  moduleServer(id, function(input, output, session) {
    
    # Identification
    output$employee_name <- renderText({identification$employee_name})
    output$department <- renderText({identification$department})
    output$fte <- renderText({identification$fte})
    output$comment <- renderText({identification$comment})
    
    
    # Section I
    
    
    output$section_i <- renderText({
      paste(section_i$publist, collapse = "<br>")
      })
    
    
    # Section III
    
    ## Undergrad
    
    
        output$section_iii_undergrad <- renderText({
          paste(reactiveValuesToList(section_iii_undergrad))
      })
        
        output$section_iii_postgrad <- renderText({
          paste(reactiveValuesToList(section_iii_postgrad))
        })
        
    
    
   
    # output$conference_foreign <- renderText({conference_foreign()})
    # output$conference_local <- renderText({conference_local()})
    
    
    
  }
  
  )
}

    
