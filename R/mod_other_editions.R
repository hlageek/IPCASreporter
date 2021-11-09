#' other_editions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_other_editions_ui <- function(id){
  ns <- NS(id)
  
  fluidRow(column(width = 6,
                  
                  textInput(ns("other_editions_name"), 
                            label = "Název díla"),
                  
                  textAreaInput(ns("other_editions_info"), 
                            label = "Doplňující informace"),
                  
                  actionButton(ns("add"),
                               label = "Add to report",                  icon = icon("check"),                  class = "btn-success"
                  )
                  
  ),
  
  column(width = 6, 
         

         htmlOutput(ns("section_ix_editions"), inline = FALSE),
         
         selectInput(ns("remove_list"), 
                     label = "Item",
                     choices = ""),
         
         br(), br(),
         actionButton(ns("remove"),
                      label = "Remove item from report"
         )
         
         
  )
  
  )
  

}
    
#' other_editions Server Function
#'
#' @noRd 
mod_other_editions_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    section_ix_editions <- reactiveValues()
    
    items <- c(
      "other_editions_name",
      "other_editions_info"
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
      
      

      section_ix_editions$editions[[
        length(
          section_ix_editions$editions)+1]] <- paste(c(all_items,"<br>"), collapse = "<br>")
      
      updateSelectInput(session = session,
                        "remove_list", 
                        choices = seq_along(section_ix_editions$editions)
      ) 
     
    })
    
    observeEvent(input$remove, {
      
      
      section_ix_editions$editions[as.integer(input$remove_list)] <- NULL 
      
      
      updateSelectInput(session = session,
                        "remove_list", 
                        choices = seq_along(section_ix_editions$editions)
                        
      )
      
    })
    
    
    output$section_ix_editions<- renderText({
      if (length(section_ix_editions$editions)>0) {
        paste(paste0(seq_along(section_ix_editions$editions), ".<br>"),
              section_ix_editions$editions)
      } else {""}
    })
    
    
    return(section_ix_editions)
    
  })}

 
