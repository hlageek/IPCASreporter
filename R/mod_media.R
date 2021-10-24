#' media UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_media_ui <- function(id){
  ns <- NS(id)
  fluidRow(column(width = 6,
                  
    
    textInput(ns("title"), label = "Název pořadu nebo textu"),
    textInput(ns("name"), label = "Médium"),
    textAreaInput(ns("description"), label = "Doplňující informace" ),
    
    actionButton(ns("add"),
                 label = "Add to report"
    )
    
    
  ),
  
  column(width = 6,

         htmlOutput(ns("section_vi_media"), inline = FALSE),
         
         selectInput(ns("remove_list"), 
                     label = "Item",
                     choices = ""),
         actionButton(ns("remove"),
                      label = "Remove item from report"
         )
    
  )
 
  )
}
    
#' media Server Function
#'
#' @noRd 
mod_media_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    section_vi_media <- reactiveValues()
    
    items <- c(
      "title",
      "name",
      "description"
    )
    
    item_names <- c(
      "Název:",
      "Médium:",
      "Dopňující informace:"
    )
    
    item_values <- reactive({
      
      unlist(purrr::map(reactiveValuesToList(input)[items], as.character))
      
    })
    
    
    observeEvent(input$add, {
      
      all_items <- list()
      
      for (i in seq_along(items)) {
        
        all_items <- c(all_items, paste(item_names[i], item_values()[i]))
        
      }
      
      
      
      section_vi_media$media[[
        length(
          section_vi_media$media)+1]] <- paste(c(all_items,"<br>"), collapse = "<br>")
      
      updateSelectInput(session = session,
                        "remove_list", 
                        choices = seq_along(section_vi_media$media)
      )
    })
    
    observeEvent(input$remove, {
      
      
      section_vi_media$media[as.integer(input$remove_list)] <- NULL 
      
      
      updateSelectInput(session = session,
                        "remove_list", 
                        choices = seq_along(section_vi_media$media)
                        
      )
      
    })
    
    
    output$section_vi_media <- renderText({
      if (length(section_vi_media$media)>0) {
        paste(paste0(seq_along(section_vi_media$media), ".<br>"),
              section_vi_media$media)
      } else {""}
    })
    
    return(section_vi_media)
    
  })}
    

 