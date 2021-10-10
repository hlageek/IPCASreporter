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
    htmlOutput(ns("section_iii_conference_foreign"), inline = FALSE),
    htmlOutput(ns("section_iii_conference_domestic"), inline = FALSE),
    
    br(),
    htmlOutput(ns("section_iii_lecture_foreign"), inline = FALSE),
    htmlOutput(ns("section_iii_lecture_domestic"), inline = FALSE),
    
    br(),
    htmlOutput(ns("section_iv_funded"), inline = FALSE),
    htmlOutput(ns("section_iv_unfunded"), inline = FALSE)
    
        
        
        
)
  
}
    
#' preview Server Function
#'
#' @noRd 
mod_preview_server <- function(id,
                               identification,
                               section_i,
                               section_iii_undergrad,
                               section_iii_postgrad,
                               section_iii_conference,
                               section_iii_lecture,
                               section_iv) {
  
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
          paste(section_iii_undergrad$data)
      })
        
    ## Postgrad
        
        output$section_iii_postgrad <- renderText({
          paste(section_iii_postgrad$data)
        })
        
    ## Conference
        
        output$section_iii_conference_foreign <-  renderText({
          paste(section_iii_conference$foreign)
        })
        output$section_iii_conference_domestic <-  renderText({
          paste(section_iii_conference$domestic)
        })
        
    ## Lecture
        
        output$section_iii_lecture_foreign <-  renderText({
          paste(section_iii_lecture$foreign)
        })
        output$section_iii_lecture_domestic <-  renderText({
          paste(section_iii_lecture$domestic)
        })
        
        
    # Section IV
        
        output$section_iv_funded <-  renderText({
          paste(section_iv$funded)
        })
        output$ssection_iv_unfunded <-  renderText({
          paste(section_iv$unfunded)
        })
        
    
    
  }
  
  )
}

    
