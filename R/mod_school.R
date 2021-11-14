#' school UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_school_ui <- function(id){
  ns <- NS(id)
  fluidRow(column(width = 6,
                  
    
    textInput(ns("title"), label = "Název přednášky či specifikace jiného druhu akce"),
    textInput(ns("name"), label = "Pořadatel/škola"),
    textAreaInput(ns("description"), label = "Popis činnosti" ),
    
    actionButton(ns("add"),
                 label = "Add to report",                  icon = icon("check"),                  class = "btn-success"
    )
    
    
  ),
  
  column(width = 6,

         htmlOutput(ns("section_vi_school"), inline = FALSE),
         
         selectInput(ns("remove_list"), 
                     label = "Item",
                     choices = ""),
         actionButton(ns("remove"),
                      label = "Remove from report", class = "btn-primary", icon = icon("trash")
         )
    
  )
 
  )
}
    
#' school Server Function
#'
#' @noRd 
mod_school_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    section_vi_school <- reactiveValues()
    
    items <- c(
      "title",
      "name",
      "description"
    )
    
    item_names <- c(
      "Název přednášky či specifikace jiného druhu akce:",
      "Pořadatel/škola:",
      "Popis činnosti:"
    )
    
    item_values <- reactive({
      
      unlist(purrr::map(reactiveValuesToList(input)[items], as.character))
      
    })
    
    
    observeEvent(input$add, {
      
      all_items <- list()
      
      for (i in seq_along(items)) {
        
        all_items <- c(all_items, paste(item_names[i], item_values()[i]))
        
      }
      
      
      
      section_vi_school$events[[
        length(
          section_vi_school$events)+1]] <- paste(c(all_items,"<br>"), collapse = "<br>")
      
      updateSelectInput(session = session,
                        "remove_list", 
                        choices = seq_along(section_vi_school$events)
      )
    })
    
    observeEvent(input$remove, {
      
      
      section_vi_school$events[as.integer(input$remove_list)] <- NULL 
      
      
      updateSelectInput(session = session,
                        "remove_list", 
                        choices = seq_along(section_vi_school$events)
                        
      )
      
    })
    
    
    output$section_vi_school <- renderText({
      if (length(section_vi_school$events)>0) {
        paste(paste0(seq_along(section_vi_school$events), ".<br>"),
              section_vi_school$events)
      } else {""}
    })
    
    # Save extra values in state$values when we bookmark
    onBookmark(function(state) {
        state$values$section_vi_school <- section_vi_school$events[-length(section_vi_school$events)]
    })
    
    # Read values from state$values when we restore
    onRestore(function(state) {
        section_vi_school$events <- state$values$section_vi_school 
    })
    
    return(section_vi_school)
    
  })}
    

 
