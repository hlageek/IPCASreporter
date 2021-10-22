#' other_award UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_other_award_ui <- function(id){
  ns <- NS(id)
  fluidRow(column(width = 6,
    
    textAreaInput(ns("other_award_name"), label = "Název ocenění"),
    
    actionButton(ns("add"),
                 label = "Add to report"
    )
    
  ),
  
  column(width = 6, 
    
         htmlOutput(ns("section_ix_award"), inline = FALSE),
         
         selectInput(ns("remove_list"), 
                     label = "Item",
                     choices = ""),
         actionButton(ns("remove"),
                      label = "Remove item from report"
         )
         
         
  )
 
  )
}
    
#' other_award Server Function
#'
#' @noRd 
mod_other_award_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    section_ix_award <- reactiveValues()
    
    items <- c(
      "other_award_name"
    )
    
    item_names <- c(
      "Název ocenění:"
    )
    
    item_values <- reactive({
      
      unlist(purrr::map(reactiveValuesToList(input)[items], as.character))
      
    })
    
    
    observeEvent(input$add, {
      
      all_items <- list()
      
      for (i in seq_along(items)) {
        
        all_items <- c(all_items, paste(item_names[i], item_values()[i]))
        
      }
      
      
      
      section_ix_award$award[[
        length(
          section_ix_award$award)+1]] <- paste(c(all_items,"<br>"), collapse = "<br>")
      
      updateSelectInput(session = session,
                        "remove_list", 
                        choices = seq_along(section_ix_award$award)
      )
    })
    
    observeEvent(input$remove, {
      
      
      section_ix_award$award[as.integer(input$remove_list)] <- NULL 
      
      
      updateSelectInput(session = session,
                        "remove_list", 
                        choices = seq_along(section_ix_award$award)
                        
      )
      
    })
    
    
    output$section_ix_award <- renderText({
      if (length(section_ix_award$award)>0) {
        paste(paste0(seq_along(section_ix_award$award), ".<br>"),
              section_ix_award$award)
      } else {""}
    })
    
    return(section_ix_award)
    
  })}
    

 
