#' conference UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_conference_ui <- function(id){
  ns <- NS(id)
  
  fluidRow(column(width = 6,
                  
                  textInput(ns("conference_contribution"), label = "Název příspěvku"),
                  textInput(ns("conference_organizer"), label = "Pořadatel"),
                  textInput(ns("conference_name"), label = "Název konference"),
                  dateInput(ns("conference_date"), label = "Datum konání"),
                  radioButtons(ns("conference_location"), label = "Kategorie", 
                               choices = c("Domácí", "Zahraniční"), inline = TRUE),
                  
                  actionButton(ns("add"),
                               label = "Add to report"
                  )
                  
  ),
  
  column(width = 6,
         
         
         h3("2)	Příspěvky a přednášky na konferencích:"),
         h4("a) Zahraniční:"),
         
         htmlOutput(ns("section_iii_conferences_foreign"), inline = FALSE),
         
         h4("b) Domácí:"),
         htmlOutput(ns("section_iii_conferences_domestic"), inline = FALSE)
         
         
  )
  )
  tagList(
    
  
    actionButton(ns("add"),
                 label = "Add to report"
    )
    

  )
  
}
    
#' conference Server Function
#'
#' @noRd 
mod_conference_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$add, {
      
      all_items <- list()
      
      for (i in seq_along(items)) {
        
        all_items <- c(all_items, paste(item_names[i], input[[items[i]]]))
        
      }
      
      if (!exists("section_iii_conferences")) {
        
        section_iii_conferences <- list()
        
      }
      
      
      section_iii_conferences[[as.character(
        length(
          reactiveValuesToList(
            section_iii_conferences))+1)
      ]] <- paste(c(all_items,"<br>"), collapse = "<br>")
      
      
    })
    
    
  }
  )
}

 
