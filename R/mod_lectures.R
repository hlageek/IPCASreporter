#' lectures UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_lectures_ui <- function(id){
  ns <- NS(id)
  fluidRow(column(width = 6,
 
    textInput(ns("lecture_contribution"), label = "Název přednášky"),
    textInput(ns("lecture_organizer"), label = "Pořadatel"),
    textInput(ns("lecture_name"), label = "Název akce"),
    dateInput(ns("lecture_date"), label = "Datum konání"),
    radioButtons(ns("lecture_location"), label = "Kategorie", 
                 choices = c("Domácí" = "domestic", 
                             "Zahraniční" = "foreign")),
    actionButton(ns("add"),
                 label = "Add to report"
    )
  ),
  
    column(width = 6,
           
           h3("3)	 Samostatné přednášky:"),
           
           h4("a) Zahraniční:"),
           
           htmlOutput(ns("section_iii_lectures_foreign"), inline = FALSE),
           
           selectInput(ns("remove_list_foreign"), 
                       label = "Item",
                       choices = ""),
           actionButton(ns("remove_foreign"),
                        label = "Remove item from report"
           ),
           
           h4("b) Domácí:"),
           htmlOutput(ns("section_iii_lectures_domestic"), inline = FALSE),
           
           selectInput(ns("remove_list_domestic"), 
                       label = "Item",
                       choices = ""),
           actionButton(ns("remove_domestic"),
                        label = "Remove item from report"
           )
           
           
           )
    
  )
}
    
#' lectures Server Function
#'
#' @noRd 
mod_lectures_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    section_iii_lecture <- reactiveValues()
    all_items <- list()
    
    
    items <- c(
      "lecture_contribution",
      "lecture_organizer",
      "lecture_name",
      "lecture_date"
    )
    
    item_names <- c(
      "Název přednášky:",
      "Pořadatel:",
      "Název akce:",
      "Datum konání:"
    )
    
    item_values <- reactive({
      
      unlist(purrr::map(reactiveValuesToList(input)[items], as.character))
      
    })
    
    
    observeEvent(input$add, {
      
      
      for (i in seq_along(items)) {
        
        all_items <- c(all_items, paste(item_names[i], item_values()[i]))
        
      }
      
      
      if (input$lecture_location == "domestic") {
        
        section_iii_lecture$domestic[[
          length(
            section_iii_lecture$domestic)+1]] <- paste(c(all_items,"<br>"), collapse = "<br>")
        
        updateSelectInput(session = session,
                          "remove_list_domestic", 
                          choices = seq_along(section_iii_lecture$domestic)
        )
        
      } else {
        
        section_iii_lecture$foreign[[as.character(
          length(
            section_iii_lecture$foreign)+1)
        ]] <- paste(c(all_items,"<br>"), collapse = "<br>")
        
        updateSelectInput(session = session,
                          "remove_list_foreign", 
                          choices = seq_along(section_iii_lecture$foreign)
        )
        
      }
      
      
      
    })
    
    observeEvent(input$remove_domestic, {
      
      
      section_iii_lecture$domestic[as.integer(input$remove_list_domestic)] <- NULL 
      
      
      updateSelectInput(session = session,
                        "remove_list_domestic", 
                        choices = seq_along(section_iii_lecture$domestic)
                        
      )
      
    })
    
    observeEvent(input$remove_foreign, {
      
      
      section_iii_lecture$foreign[as.integer(input$remove_list_foreign)] <- NULL 
      
      
      updateSelectInput(session = session,
                        "remove_list_foreign", 
                        choices = seq_along(section_iii_lecture$foreign)
                        
      )
      
    })
    
    output$section_iii_lectures_domestic <- renderText({
      if (length(section_iii_lecture$domestic)>0) {
        paste(paste0(seq_along(section_iii_lecture$domestic), ".<br>"),
              section_iii_lecture$domestic)
      } else {""}
    })
    
    output$section_iii_lectures_foreign <- renderText({
      if (length(section_iii_lecture$foreign)>0) {
        paste(paste0(seq_along(section_iii_lecture$foreign), ".<br>"),
              section_iii_lecture$foreign)
      } else {""}
    })    
    
    
    return(section_iii_lecture)
    
  })}
 

    