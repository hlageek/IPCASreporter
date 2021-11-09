#' av21 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_av21_ui <- function(id){
  ns <- NS(id)
  
  fluidRow(column(width = 6,
                  
    
      textInput(ns("program"), label = "Program Strategie AV21"),
      textInput(ns("activity"), label = "Název aktivity (projektu)"),
      textInput(ns("person"), label = "Řešitel aktivity (projektu)"),
      textAreaInput(ns("annotation_cze"), label = "Anotace česky",
                    placeholder = "Lze zkopírovat z návrhového listu aktivity"),
      textAreaInput(ns("annotation_eng"), label = "Anotace anglicky"),
      textAreaInput(ns("results"), label = "Výstupy (včetně příp. odkazu na ASEP)"),

      textInput(ns("partner"), label = "Spolupracující instituce"),


      actionButton(ns("add"),
                   label = "Add to report",                  icon = icon("check"),                  class = "btn-success"
      )
    
  ),
  
  column(width = 6,
         
         htmlOutput(ns("section_v"), inline = FALSE),
         
         selectInput(ns("remove_list"), 
                     label = "Item",
                     choices = ""),
         actionButton(ns("remove"),
                      label = "Remove from report", class = "btn-primary", icon = icon("trash")
         )
         
         
  )
  )
}
    
#' av21 Server Function
#'
#' @noRd 
mod_av21_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    section_v <- reactiveValues()
    
    items <- c(
      "program",
      "activity",
      "person",
      "annotation_cze",
      "annotation_eng",
      "results",
      "partner"
    )
    
    item_names <- c(
      "Program Strategie AV21:",
      "Název aktivity (projektu)",
      "Řešitel aktivity (projektu):",
      "Anotace česky:",
      "Anotace anglicky:",
      "Výstupy (včetně příp. odkazu na ASEP):",
      "Spolupracující instituce:"
    )
    
    item_values <- reactive({
      
      unlist(purrr::map(reactiveValuesToList(input)[items], as.character))
      
    })
    
    
    observeEvent(input$add, {
      
      all_items <- list()
      
      for (i in seq_along(items)) {
        
        all_items <- c(all_items, paste(item_names[i], item_values()[i]))
        
      }
      
      

        section_v$av21[[
          length(
            section_v$av21)+1]] <- paste(c(all_items,"<br>"), collapse = "<br>")
        
        updateSelectInput(session = session,
                          "remove_list", 
                          choices = seq_along(section_v$av21)
        )
    })
    
    observeEvent(input$remove, {
      
      
      section_v$av21[as.integer(input$remove_list)] <- NULL 
      
      
      updateSelectInput(session = session,
                        "remove_list", 
                        choices = seq_along(section_v$av21)
                        
      )
      
    })
    
    
    output$section_v <- renderText({
      if (length(section_v$av21)>0) {
        paste(paste0(seq_along(section_v$av21), ".<br>"),
              section_v$av21)
      } else {""}
    })
    
    return(section_v)
    
  })}
 

    

 
