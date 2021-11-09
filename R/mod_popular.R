#' popular UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_popular_ui <- function(id){
  ns <- NS(id)
  
  fluidRow(column(width = 6,
                  
    

    textInput(ns("contribution"), label = "Název akce"),
    textInput(ns("description"), label = "Popis aktivity"),
    textInput(ns("organizer_primary"), label = "Hlavní pořadatel"),
    textInput(ns("organizer_secondary"), label = "Spolupořadatel"),
    textInput(ns("place"), label = "Místo konání akce"),
    dateInput(ns("date"), label = "Datum konání akce"),
    
    actionButton(ns("add"),
                 label = "Add to report",                  icon = icon("check"),                  class = "btn-success"
    )
    

  ),
  
  column(width = 6,
          
          htmlOutput(ns("section_vi_popular"), inline = FALSE),
          
          selectInput(ns("remove_list"), 
                      label = "Item",
                      choices = ""),
          actionButton(ns("remove"),
                       label = "Remove item from report"
          )
          
          
  )
  )
}
    
#' popular Server Function
#'
#' @noRd 
mod_popular_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    section_vi_popular <- reactiveValues()
    
    items <- c(
      "contribution",
      "description",
      "organizer_primary",
      "organizer_secondary",
      "place",
      "date"
      )
    
    item_names <- c(
      "Název akce:",
      "Popis aktivity:",
      "Hlavní pořadatel:",
      "Spolupořadatel:",
      "Místo konání akce:",
      "Datum konání akce:"
    )
    
    item_values <- reactive({
      
      unlist(purrr::map(reactiveValuesToList(input)[items], format_input))
      
    })
    
    
    observeEvent(input$add, {
      
      all_items <- list()
      
      for (i in seq_along(items)) {
        
        all_items <- c(all_items, paste(item_names[i], item_values()[i]))
        
      }
      
      
      
      section_vi_popular$events[[
        length(
          section_vi_popular$events)+1]] <- paste(c(all_items,"<br>"), collapse = "<br>")
      
      updateSelectInput(session = session,
                        "remove_list", 
                        choices = seq_along(section_vi_popular$events)
      )
    })
    
    observeEvent(input$remove, {
      
      
      section_vi_popular$events[as.integer(input$remove_list)] <- NULL 
      
      
      updateSelectInput(session = session,
                        "remove_list", 
                        choices = seq_along(section_vi_popular$events)
                        
      )
      
    })
    
    
    output$section_vi_popular <- renderText({
      if (length(section_vi_popular$events)>0) {
        paste(paste0(seq_along(section_vi_popular$events), ".<br>"),
              section_vi_popular$events)
      } else {""}
    })
    
    return(section_vi_popular)
   
  })}
    

