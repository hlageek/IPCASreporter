#' wip UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_wip_ui <- function(id){
  ns <- NS(id)
  fluidRow(column(width = 6,
    
    textAreaInput(ns("wip_description"), label = "Popis"),
    
    
    actionButton(ns("add"),
                 label = "Zadat do výkazu",                  icon = icon("check"),                  class = "btn-success"
    )
    
  ),
  
  column(width = 6, 
         
         htmlOutput(ns("section_x"), inline = FALSE),
         
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
    
#' wip Server Function
#'
#' @noRd 
mod_wip_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    section_x <- reactiveValues()
    
    items <- c(
      "wip_description"
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
      
      
      
      section_x$wip[[
        length(
          section_x$wip)+1]] <- paste(c(all_items,"<br>"), collapse = "<br>")
      
      updateSelectInput(session = session,
                        "remove_list", 
                        choices = seq_along(section_x$wip)
      )
    })
    
    observeEvent(input$remove, {
      
      
      section_x$wip[as.integer(input$remove_list)] <- NULL 
      
      
      updateSelectInput(session = session,
                        "remove_list", 
                        choices = seq_along(section_x$wip)
                        
      )
      
    })
    
    
    output$section_x <- renderText({
      if (length(section_x$wip)>0) {
        paste(paste0(seq_along(section_x$wip), ".<br>"),
              section_x$wip)
      } else {""}
    })
    
    
    # Save extra values in state$values when we bookmark
    onBookmark(function(state) {
        state$values$wip <- section_x$wip[-length(section_x$wip)]
    })
    # Read values from state$values when we restore
    onRestore(function(state) {
        section_x$wip <- state$values$wip
    })
    
    return(section_x)
    
  })}
    

 
