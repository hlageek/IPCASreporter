#' various UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_various_ui <- function(id){
  ns <- NS(id)
  fluidRow(column(width = 6,
                  
    
    textAreaInput(ns("various_description"), label = "Popis"),
    
    
    actionButton(ns("add"),
                 label = "Zadat do výkazu",                  icon = icon("check"),                  class = "btn-success"
    )
    
  ),
  
  column(width = 6, 
         
         htmlOutput(ns("section_xi"), inline = FALSE),
         
         selectInput(ns("remove_list"), 
                     label = "Položka",
                     choices = ""),
         
         br(), br(),
         actionButton(ns("remove"),
                      label = "Odstranit z výkazu", class = "btn-primary", icon = icon("trash")
         )
  ) 
  )
}
    
#' various Server Function
#'
#' @noRd 
mod_various_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    section_xi <- reactiveValues()
    
    items <- c(
      "various_description"
    )
    
    item_names <- c(
      "Popis:"
    )
    
    item_values <- reactive({
      
      unlist(purrr::map(reactiveValuesToList(input)[items], as.character))
      
    })
    
    
    observeEvent(input$add, {
      
      all_items <- list()
      
      for (i in seq_along(items)) {
        
        all_items <- c(all_items, paste(item_names[i], item_values()[i]))
        
      }
      
      
      
      section_xi$data[[
        length(
          section_xi$data)+1]] <- paste(c(all_items,"<br>"), collapse = "<br>")
      
      updateSelectInput(session = session,
                        "remove_list", 
                        choices = seq_along(section_xi$data)
      )
    })
    
    observeEvent(input$remove, {
      
      
      section_xi$data[as.integer(input$remove_list)] <- NULL 
      
      
      updateSelectInput(session = session,
                        "remove_list", 
                        choices = seq_along(section_xi$data)
                        
      )
      
    })
    
    
    output$section_xi <- renderText({
      if (length(section_xi$data)>0) {
        paste(paste0(seq_along(section_xi$data), ".<br>"),
              section_xi$data)
      } else {""}
    })
    
    
    # Save extra values in state$values when we bookmark
    onBookmark(function(state) {
        state$values$section_xi <- section_xi$data[-length(section_xi$data)]
    })
    
    # Read values from state$values when we restore
    onRestore(function(state) {
        section_xi$data <- state$values$section_xi 
    })
    
    return(section_xi)
    
  })}
    
## To be copied in the UI
# mod_various_ui("various_ui_1")
    
## To be copied in the server
# mod_various_server("various_ui_1")
 
