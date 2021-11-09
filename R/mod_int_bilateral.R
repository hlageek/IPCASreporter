#' int_bilateral UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_int_bilateral_ui <- function(id){
  ns <- NS(id)
  
  fluidRow(column(width = 6,
                  
                  textInput(ns("int_bilateral_description"), label = "Bilaterální spolupráce"),
                  
                  actionButton(ns("add"),
                               label = "Add to report",                  icon = icon("check"),                  class = "btn-success"
                  )
                  
  ),
  
  column(width = 6,    
         
         htmlOutput(ns("section_viii_int_bilateral"), inline = FALSE),
         
         selectInput(ns("remove_list"), 
                     label = "Item",
                     choices = ""),
         actionButton(ns("remove"),
                      label = "Remove item from report"
         )
         
         
  )
  )

}
    
#' int_bilateral Server Function
#'
#' @noRd 
mod_int_bilateral_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    section_viii_int_bilateral <- reactiveValues()
    
    items <- c(
      "int_bilateral_description"
    )
    
    item_names <- c(
      "Bilaterální spolupráce:"
    )
    
    item_values <- reactive({
      
      unlist(purrr::map(reactiveValuesToList(input)[items], as.character))
      
    })
    
    
    observeEvent(input$add, {
      
      all_items <- list()
      
      for (i in seq_along(items)) {
        
        all_items <- c(all_items, paste(item_names[i], item_values()[i]))
        
      }
      
      
      
      section_viii_int_bilateral$bilateral[[
        length(
          section_viii_int_bilateral$bilateral)+1]] <- paste(c(all_items,"<br>"), collapse = "<br>")
      
      updateSelectInput(session = session,
                        "remove_list", 
                        choices = seq_along(section_viii_int_bilateral$bilateral)
      )
    })
    
    observeEvent(input$remove, {
      
      
      section_viii_int_bilateral$bilateral[as.integer(input$remove_list)] <- NULL 
      
      
      updateSelectInput(session = session,
                        "remove_list", 
                        choices = seq_along(section_viii_int_bilateral$bilateral)
                        
      )
      
    })
    
    
    output$section_viii_int_bilateral <- renderText({
      if (length(section_viii_int_bilateral$bilateral)>0) {
        paste(paste0(seq_along(section_viii_int_bilateral$bilateral), ".<br>"),
              section_viii_int_bilateral$bilateral)
      } else {""}
    })
    
    return(section_viii_int_bilateral)
    
  })}

    

 
