#' public UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_public_ui <- function(id){
  ns <- NS(id)
  fluidRow(column(width = 6,
                  
    textInput(ns("body"), label = "Instituce státní nebo veřejné správy"),
    textAreaInput(ns("description"), label = "Popis spolupráce"),
    
    actionButton(ns("add"),
                 label = "Add to report",                  icon = icon("check"),                  class = "btn-success"
    )
    
    
  ),
  
  column(width = 6,
    
         
         htmlOutput(ns("section_vii"), inline = FALSE),
         
         selectInput(ns("remove_list"), 
                     label = "Item",
                     choices = ""),
         actionButton(ns("remove"),
                      label = "Remove item from report"
         )
         
  )
  )
}
    
#' public Server Function
#'
#' @noRd 
mod_public_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    section_vii <- reactiveValues()
    
    items <- c(
      "body",
      "description"
    )
    
    item_names <- c(
      "Instituce státní nebo veřejné správy:",
      "Popis spolupráce:"
    )
    
    item_values <- reactive({
      
      unlist(purrr::map(reactiveValuesToList(input)[items], as.character))
      
    })
    
    
    observeEvent(input$add, {
      
      all_items <- list()
      
      for (i in seq_along(items)) {
        
        all_items <- c(all_items, paste(item_names[i], item_values()[i]))
        
      }
      
      
      
      section_vii$public[[
        length(
          section_vii$public)+1]] <- paste(c(all_items,"<br>"), collapse = "<br>")
      
      updateSelectInput(session = session,
                        "remove_list", 
                        choices = seq_along(section_vii$public)
      )
    })
    
    observeEvent(input$remove, {
      
      
      section_vii$public[as.integer(input$remove_list)] <- NULL 
      
      
      updateSelectInput(session = session,
                        "remove_list", 
                        choices = seq_along(section_vii$public)
                        
      )
      
    })
    
    
    output$section_vii <- renderText({
      if (length(section_vii$public)>0) {
        paste(paste0(seq_along(section_vii$public), ".<br>"),
              section_vii$public)
      } else {""}
    })
    
    return(section_vii)
    
  })}
    

