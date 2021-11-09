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
                  
                  textInput(ns("conference_contribution"), 
                            label = "Název příspěvku"),
                  
                  textInput(ns("conference_organizer"), 
                            label = "Pořadatel"),
                  
                  textInput(ns("conference_name"), 
                            label = "Název konference"),
                  
                  dateRangeInput(ns("conference_date"), 
                            label = "Datum konání",
                            language = "cs"),
                  
                  radioButtons(ns("conference_location"), 
                               label = "Kategorie", 
                               choices = c("Domácí" = "domestic",
                                           "Zahraniční" = "foreign")
                               ),
                  
                  

                  actionButton(ns("add"),
                               label = "Add to report",                  icon = icon("check"),                  class = "btn-success"
                  )
  ),
  
  column(width = 6,
         
         
         h3("2)	Příspěvky a přednášky na konferencích:"),
         h4("a) Zahraniční:"),
         
         htmlOutput(ns("section_iii_conferences_foreign"), inline = FALSE),
         
         selectInput(ns("remove_list_foreign"), 
                     label = "Item",
                     choices = ""),
         actionButton(ns("remove_foreign"),
                      label = "Remove item from report"
         ),
         
         h4("b) Domácí:"),
         htmlOutput(ns("section_iii_conferences_domestic"), inline = FALSE),
         
         selectInput(ns("remove_list_domestic"), 
                     label = "Item",
                     choices = ""),
         actionButton(ns("remove_domestic"),
                      label = "Remove item from report"
         )
         
  )
  )
  
  
}
    
#' conference Server Function
#'
#' @noRd 
mod_conference_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    section_iii_conference <- reactiveValues()
   
    
    
    items <- c(
      "conference_contribution",
      "conference_organizer",
      "conference_name",
      "conference_date"
    )
    
    item_names <- c(
      "Název příspěvku:",
      "Pořadatel:",
      "Název konference:",
      "Datum konání:"
    )
    
    
    item_values <- reactive({

      unlist(purrr::map(reactiveValuesToList(input)[items], format_input))
      
    })
    
    
    observeEvent(input$add, {
      #browser()
      all_items <- list()
      
      for (i in seq_along(items)) {
        
        all_items <- c(all_items, paste(item_names[i], item_values()[i]))
        
      }
      

      if (input$conference_location == "domestic") {
        
      section_iii_conference$domestic[[
        length(
          section_iii_conference$domestic)+1]] <- paste(c(all_items,"<br>"), collapse = "<br>")
      
      updateSelectInput(session = session,
                        "remove_list_domestic", 
                        choices = seq_along(section_iii_conference$domestic)
      )
      
      } else {
        
        section_iii_conference$foreign[[as.character(
          length(
              section_iii_conference$foreign)+1)
        ]] <- paste(c(all_items,"<br>"), collapse = "<br>")
        
        updateSelectInput(session = session,
                          "remove_list_foreign", 
                          choices = seq_along(section_iii_conference$foreign)
        )
        
      }
      
   
      
    })
    
    observeEvent(input$remove_domestic, {
      
      
      section_iii_conference$domestic[as.integer(input$remove_list_domestic)] <- NULL 
      

      updateSelectInput(session = session,
                        "remove_list_domestic", 
                        choices = seq_along(section_iii_conference$domestic)
                        
      )
      
    })
    
    observeEvent(input$remove_foreign, {
      
      
      section_iii_conference$foreign[as.integer(input$remove_list_foreign)] <- NULL 
      
      
      updateSelectInput(session = session,
                        "remove_list_foreign", 
                        choices = seq_along(section_iii_conference$foreign)
                        
      )
      
    })
    
    output$section_iii_conferences_domestic <- renderText({
      if (length(section_iii_conference$domestic)>0) {
        paste(paste0(seq_along(section_iii_conference$domestic), ".<br>"),
              section_iii_conference$domestic)
      } else {""}
    })
    
    output$section_iii_conferences_foreign <- renderText({
      if (length(section_iii_conference$foreign)>0) {
        paste(paste0(seq_along(section_iii_conference$foreign), ".<br>"),
              section_iii_conference$foreign)
      } else {""}
    })
    
    
    return(section_iii_conference)
    
    
  }
  )
}

 
