#' int_projects UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_int_projects_ui <- function(id){
  ns <- NS(id)
  
  fluidRow(column(width = 6,
    
    textInput(ns("int_projects_name"), label = "Název projektu",
              placeholder = "např. INTER-EXCELLENCE"),

    actionButton(ns("add"),
                 label = "Add to report",                  icon = icon("check"),                  class = "btn-success"
    )
    
    ),
    
    column(width = 6,    
 
           htmlOutput(ns("section_viii_int_projects"), inline = FALSE),
           
           selectInput(ns("remove_list"), 
                       label = "Item",
                       choices = ""),
           actionButton(ns("remove"),
                        label = "Remove from report", class = "btn-primary", icon = icon("trash")
           )
           
           
    )
  )
}
    
#' int_projects Server Function
#'
#' @noRd 
mod_int_projects_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    section_viii_int_projects <- reactiveValues()
    
    items <- c(
      "int_projects_name"
    )
    
    item_names <- c(
      "Název projektu:"
    )
    
    item_values <- reactive({
      
      unlist(purrr::map(reactiveValuesToList(input)[items], as.character))
      
    })
    
    
    observeEvent(input$add, {
      
      all_items <- list()
      
      for (i in seq_along(items)) {
        
        all_items <- c(all_items, paste(item_names[i], item_values()[i]))
        
      }
      
      
      
      section_viii_int_projects$projects[[
        length(
          section_viii_int_projects$projects)+1]] <- paste(c(all_items,"<br>"), collapse = "<br>")
      
      updateSelectInput(session = session,
                        "remove_list", 
                        choices = seq_along(section_viii_int_projects$projects)
      )
    })
    
    observeEvent(input$remove, {
      
      
      section_viii_int_projects$projects[as.integer(input$remove_list)] <- NULL 
      
      
      updateSelectInput(session = session,
                        "remove_list", 
                        choices = seq_along(section_viii_int_projects$projects)
                        
      )
      
    })
    
    
    output$section_viii_int_projects <- renderText({
      if (length(section_viii_int_projects$projects)>0) {
        paste(paste0(seq_along(section_viii_int_projects$projects), ".<br>"),
              section_viii_int_projects$projects)
      } else {""}
    })
    
    return(section_viii_int_projects)
    
  })}

    

 
