#' other_member UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_other_member_ui <- function(id){
  ns <- NS(id)
  
  fluidRow(column(width = 6,
                  
                  textInput(ns("other_member_name"), 
                            label = "Druh grémia"),
                  
                  textInput(ns("other_member_institute"), 
                            label = "Organizace (instituce/periodikum/nakladatelství)"),
                  
                  textInput(ns("other_member_position"), 
                            label = "Funkce a funkční období"),
                  
                  radioButtons(ns("other_member_category"), 
                               label = NULL,
                               choices = c("Domácí" = "domestic",
                                           "Zahraniční" = "foreign")),
                  
                  actionButton(ns("add"),
                               label = "Add to report"
                  )
                  
  ),
  
  column(width = 6, 
         
         h3("Domácí"),
         
         htmlOutput(ns("section_ix_member_domestic"), inline = FALSE),
         
         selectInput(ns("remove_list_domestic"), 
                     label = "Item",
                     choices = ""),
         
         br(), br(),
         actionButton(ns("remove_domestic"),
                      label = "Remove item from report"
         ),
         
         h3("Zahraniční"),
         
         htmlOutput(ns("section_ix_member_foreign"), inline = FALSE),
         
         selectInput(ns("remove_list_foreign"), 
                     label = "Item",
                     choices = ""),
         
         br(), br(),
         actionButton(ns("remove_foreign"),
                      label = "Remove item from report"
         )
         
         
         
  )
  
  )
  

}
    
#' other_member Server Function
#'
#' @noRd 
mod_other_member_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    section_ix_member <- reactiveValues()
    
    items <- c(
      "other_member_name",
      "other_member_institute",
      "other_member_position"
    )
    
    item_names <- c(
      "Grémium:",
      "Organizace:",
      "Funkce:"
    )
    
    item_values <- reactive({
      
      unlist(purrr::map(reactiveValuesToList(input)[items], as.character))
      
    })
    
    
    observeEvent(input$add, {
      
      all_items <- list()
      
      for (i in seq_along(items)) {
        
        all_items <- c(all_items, paste(item_names[i], item_values()[i]))
        
      }
      
      
      if (input$other_member_category == "domestic") {
        
      section_ix_member$domestic[[
        length(
          section_ix_member$domestic)+1]] <- paste(c(all_items,"<br>"), collapse = "<br>")
      
      updateSelectInput(session = session,
                        "remove_list_domestic", 
                        choices = seq_along(section_ix_member$domestic)
      ) 
      } else {
        
      section_ix_member$foreign[[
          length(
            section_ix_member$foreign)+1]] <- paste(c(all_items,"<br>"), collapse = "<br>")
        
        updateSelectInput(session = session,
                          "remove_list_foreign", 
                          choices = seq_along(section_ix_member$foreign)
        ) 
        
      }
    })
    
    observeEvent(input$remove_domestic, {
      
      
      section_ix_member$domestic[as.integer(input$remove_list_domestic)] <- NULL 
      
      
      updateSelectInput(session = session,
                        "remove_list_domestic", 
                        choices = seq_along(section_ix_member$domestic)
                        
      )
      
    })
    
    observeEvent(input$remove_foreign, {
      
      
      section_ix_member$foreign[as.integer(input$remove_list_foreign)] <- NULL 
      
      
      updateSelectInput(session = session,
                        "remove_list_foreign", 
                        choices = seq_along(section_ix_member$foreign)
                        
      )
      
    })
    
    
    output$section_ix_member_domestic <- renderText({
      if (length(section_ix_member$domestic)>0) {
        paste(paste0(seq_along(section_ix_member$domestic), ".<br>"),
              section_ix_member$domestic)
      } else {""}
    })
    
    output$section_ix_member_foreign <- renderText({
      if (length(section_ix_member$foreign)>0) {
        paste(paste0(seq_along(section_ix_member$foreign), ".<br>"),
              section_ix_member$foreign)
      } else {""}
    })
    
    return(section_ix_member)
    
  })}

 
