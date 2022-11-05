#' events UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_events_ui <- function(id, i18n){
  ns <- NS(id)
  
  fluidRow(column(width = 4,
                  
                  
                  uiOutput(ns("events")),
                  
                  actionButton(ns("add"),
                               label = "Zadat do výkazu",
                               class = "btn-success",
                               icon = icon("check")
                  )
                  
  ),
  
  column(width = 8,
         
         h4("Events selected for report"),
         htmlOutput(ns("section_ii"), inline = FALSE),
         
         
  )
  )
  
}
    
#' events Server Function
#'
#' @noRd 
mod_events_server <- function(id, identification, usr, i18n) {
  moduleServer(id, function(input, output, session) { 
    
      ns <- NS(id)
      section_ii <- reactiveValues()
      
      observeEvent(usr$person_id, {
          
          section_ii$eventlist <- ipcas_db %>% 
              dplyr::tbl("events") %>% 
              dplyr::filter(person_id_events == !!usr$person_id) %>% 
              dplyr::pull(event)
          
      })
      
    output$events <- renderUI({
      
      if (!isTruthy(identification$employee_name)) {
        
        "Fill your identification details first."
        
      } else {
        
        citations <-  get_asep(identification$employee_name, type = "events")
        
        if (!is.null(citations)) {
          
          displayed_citations <- purrr::map(
            citations,
            ~stringr::str_replace_all(.x, "<.*?>", " ")
          )
          

          tagList(
            
            checkboxGroupInput(ns("eventlist"), 
                               label ="Most recent events found in ASEP.", 
                               width = "100%",
                               choiceNames = displayed_citations,
                               choiceValues = citations)
          )
        } else {
          
          paste0("No ASEP records found for author ", 
                 identification$employee_name, 
                 " in year ", 
                 format(Sys.Date(), "%Y"), 
                 " or ", 
                 format(Sys.Date()-365, "%Y"), 
                 ".")
        }
        
      }
      
      
    })
    
    observeEvent(input$add, {
        
        
        event_ids <- ipcas_db %>% 
            dplyr::tbl("events") %>% 
            dplyr::filter(person_id_events == !!usr$person_id) %>% 
            dplyr::pull(event_id)
        
        if (length(event_ids)>0) {
            pool::dbExecute(ipcas_db, 
                            "DELETE FROM events WHERE event_id IN (?)",
                            params = list(event_ids)
            )
        }
        
        purrr::walk(input$eventlist, .f = function(x) {
            pool::dbExecute(ipcas_db, 
                            paste0( "INSERT INTO events",
                                    " (",
                                    "person_id_events,", 
                                    "event",
                                    ")",
                                    " VALUES(",
                                    "'", usr$person_id, "',",
                                    "'", x, "'",
                                    ")"
                            )
            )
        })
        
        section_ii$eventlist <- ipcas_db %>% 
            dplyr::tbl("events") %>% 
            dplyr::filter(person_id_events == !!usr$person_id) %>% 
            dplyr::pull(event)
        
    })
    
    output$section_ii <- renderText({
        paste(section_ii$eventlist, collapse = "<br>")
    })
    
    return(section_ii)
    
  })
    }
    
