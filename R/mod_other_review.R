#' other_review UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_other_review_ui <- function(id){
  ns <- NS(id)
  
  
  fluidRow(column(width = 6,
                  
                  textInput(ns("other_review_name"), 
                            label = "Název"),
                  textAreaInput(ns("other_review_description"), 
                                label = "Doplňující informace"),
                  
                  
                  actionButton(ns("add"),
                               label = "Add to report",                  icon = icon("check"),                  class = "btn-success"
                  )
                  
  ),
  
  column(width = 6, 
         
         htmlOutput(ns("section_ix_review"), inline = FALSE),
         
         selectInput(ns("remove_list"), 
                     label = "Item",
                     choices = ""),
         
         br(), br(),
         actionButton(ns("remove"),
                      label = "Remove from report", class = "btn-primary", icon = icon("trash")
         )
         
         
  )
  
  )
  
}
    
#' other_review Server Function
#'
#' @noRd 
mod_other_review_server <- function(id) {
  moduleServer(id, function(input, output, session) {
  
    section_ix_review <- reactiveValues()
    
    items <- c(
      "other_review_name",
      "other_review_description"
    )
    
    item_names <- c(
      "Název:",
      "Doplňující informace:"
    )
    
    item_values <- reactive({
      
      unlist(purrr::map(reactiveValuesToList(input)[items], as.character))
      
    })
    
    
    observeEvent(input$add, {
      
      all_items <- list()
      
      for (i in seq_along(items)) {
        
        all_items <- c(all_items, paste(item_names[i], item_values()[i]))
        
      }
      
      
      
      section_ix_review$review[[
        length(
          section_ix_review$review)+1]] <- paste(c(all_items,"<br>"), collapse = "<br>")
      
      updateSelectInput(session = session,
                        "remove_list", 
                        choices = seq_along(section_ix_review$review)
      )
    })
    
    observeEvent(input$remove, {
      
      
      section_ix_review$review[as.integer(input$remove_list)] <- NULL 
      
      
      updateSelectInput(session = session,
                        "remove_list", 
                        choices = seq_along(section_ix_review$review)
                        
      )
      
    })
    
    
    output$section_ix_review <- renderText({
      if (length(section_ix_review$review)>0) {
        paste(paste0(seq_along(section_ix_review$review), ".<br>"),
              section_ix_review$review)
      } else {""}
    })

    
    # Save extra values in state$values when we bookmark
    onBookmark(function(state) {
        state$values$section_ix_review <- section_ix_review$review[-length(section_ix_review$review)]
    })
    
    # Read values from state$values when we restore
    onRestore(function(state) {
        section_ix_review$review <- state$values$section_ix_review 
    })
    
    return(section_ix_review)
    
  })}

 
