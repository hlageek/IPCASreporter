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
                               label = i18n$t("Zadat do výkazu"),
                               class = "btn-success",
                               icon = icon("check")
                  )
                  
  ),
  
  column(width = 8,
         
         h4(i18n$t("Events selected for report")),
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
      year <-  as.integer( format(Sys.Date(), "%Y"))
      
      observeEvent(usr$person_id, {
          
          section_ii$eventlist <- get_asep_sourced_data(ipcas_db = ipcas_db, 
                                                        tbl = "events", 
                                                        person_id = usr$person_id, 
                                                        year = year, 
                                                        col_target = "event") 
          
      })
      
    output$events <- renderUI({
      
      if (!isTruthy(identification$employee_name)) {
        
        i18n()$t("Fill your identification details first.")
        
      } else {
        
        citations <-  get_asep(identification$employee_name, type = "events")
        
        if (!is.null(citations)) {
          
          displayed_citations <- purrr::map(
            citations,
            ~stringr::str_replace_all(.x, "<.*?>", " ")
          )
          

          tagList(
            
            checkboxGroupInput(ns("eventlist"), 
                               label =i18n()$t("Nedávné záznamy nalezené v ASEP."), 
                               width = "100%",
                               choiceNames = displayed_citations,
                               choiceValues = citations)
          )
        } else {
          
          paste0(i18n()$t("V ASEP nebyly nalezeny žádné záznamy pro autora "), 
                 identification$employee_name, 
                 i18n()$t(" in year "), 
                 format(Sys.Date(), "%Y"), 
                 i18n()$t(" or "), 
                 format(Sys.Date()-365, "%Y"), 
                 ".")
        }
        
      }
      
      
    })
    
    observeEvent(input$add, {
        
        
        event_ids <-  get_asep_sourced_data(ipcas_db = ipcas_db, 
                                            tbl = "events", 
                                            person_id = usr$person_id, 
                                            year = year, 
                                            col_target = "event_id") 
   
        
        if (length(event_ids)>0) {
            pool::dbExecute(ipcas_db, 
                            "DELETE FROM events WHERE event_id IN (?)",
                            params = list(event_ids)
            )
        }
        
        new_entry_df <- tibble::tibble(
            person_id_events = usr$person_id,
            event_id_year = as.integer( format(Sys.Date(), "%Y")),
            event = input$eventlist
        )
        
        if (exists("event", new_entry_df)) {
            DBI::dbAppendTable(ipcas_db, "events", new_entry_df)
        }
        
        section_ii$eventlist <- get_asep_sourced_data(ipcas_db = ipcas_db, 
                                                      tbl = "events", 
                                                      person_id = usr$person_id, 
                                                      year = year, 
                                                      col_target = "event") 
        
    })
    
    output$section_ii <- renderText({
        paste(section_ii$eventlist, collapse = "<br>")
    })
    
    return(section_ii)
    
  })
    }
    
