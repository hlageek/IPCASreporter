#' conference UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_conference_ui <- function(id, i18n){
  ns <- NS(id)
  
  fluidRow(column(width = 6,
                  
                  textInput(ns("conference_contribution"), 
                            label = "Název příspěvku"),
                  
                  textInput(ns("conference_organizer"), 
                            label = "Pořadatel"),
                  
                  textInput(ns("conference_name"), 
                            label = "Název konference"),
                  
                  dateRangeInput(ns("conference_date"), 
                            label = "Datum konání",
                            language = "cs"),
                  
                  radioButtons(ns("conference_location"), 
                               label = "Kategorie", 
                               choices = c("Domácí" = "Domácí",
                                           "Zahraniční" = "Zahraniční")
                               ),
                  
                  

                  actionButton(ns("add"),
                               label = "Add to report",                  icon = icon("check"),                  class = "btn-success"
                  )
  ),
  
  column(width = 6,
         
         
         h3("2)	Příspěvky a přednášky na konferencích:"),
         h4("a) Zahraniční:"),
         
         htmlOutput(ns("section_iii_conferences_foreign"), inline = FALSE),
         
         selectInput(ns("remove_list_foreign"), 
                     label = "Item",
                     choices = ""),
         actionButton(ns("remove_foreign"),
                      label = "Remove from report", class = "btn-primary", icon = icon("trash")
         ),
         
         h4("b) Domácí:"),
         htmlOutput(ns("section_iii_conferences_domestic"), inline = FALSE),
         
         selectInput(ns("remove_list_domestic"), 
                     label = "Item",
                     choices = ""),
         actionButton(ns("remove_domestic"),
                      label = "Remove from report", class = "btn-primary", icon = icon("trash")
         )
         
  )
  )
  
  
}
    
#' conference Server Function
#'
#' @noRd 
mod_conference_server <- function(id, usr, i18n) {
  moduleServer(id, function(input, output, session) {
    
    
    section_iii_conference <- reactiveValues()
    loc <- reactiveValues()
    
    
    items <- c(
      "conference_contribution",
      "conference_organizer",
      "conference_name",
      "conference_date",
      "conference_location"
    )
    
    item_names <- c(
      "Název příspěvku:",
      "Pořadatel:",
      "Název konference:",
      "Datum konání:",
      "Místo konání:"
    )
    
    
    
    
    observeEvent(usr$person_id, {
        
        loc$names <- tibble::tibble(key = items,
                                    names = item_names)

           loc$all_df <-  ipcas_db %>%
            dplyr::tbl("conferences") %>%
            dplyr::filter(person_id_conferences == !!usr$person_id) %>%
            dplyr::select(-person_id_conferences) %>%
            tidyr::pivot_longer(-conference_id,
                                names_to = "key",
                                values_to = "value") %>%
            dplyr::collect() %>%
            dplyr::left_join(loc$names, by = "key") %>%
            tidyr::unite("value", c(names, value), sep = " ") %>%
            dplyr::select(-key) %>%
            dplyr::group_by(conference_id) %>%
            dplyr::summarise(data = stringr::str_flatten(value,                                                    collapse = "<br>"))
           section_iii_conference$data <- paste0("<br>",  as.list(loc$all_df$data), "<br>")
           
           

           ids_domestic <-  ipcas_db %>%
               dplyr::tbl("conferences") %>%
               dplyr::filter(person_id_conferences == !!usr$person_id) %>% 
               dplyr::filter(conference_location == "Domácí") %>% 
               dplyr::pull(conference_id)
           
           ids_foreign <- ipcas_db %>%
               dplyr::tbl("conferences") %>%
               dplyr::filter(person_id_conferences == !!usr$person_id) %>% 
               dplyr::filter(conference_location == "Zahraniční") %>% 
               dplyr::pull(conference_id)
           
        updateSelectInput(session = session,
                          "remove_list_domestic",
                          choices = stats::setNames(
                              ids_domestic,
                              seq_along(ids_domestic)))
        updateSelectInput(session = session,
                          "remove_list_foreign",
                          choices = stats::setNames(
                              ids_foreign,
                              seq_along(ids_foreign)))

        
        
    })
    
    item_values <- reactive({

      unlist(purrr::map(reactiveValuesToList(input)[items], format_input))
      
    })
    
    
    observeEvent(input$add, {
browser()
          all_items <- purrr::map2_chr(items, item_names,
                                       .f = function(items, item_names) {
              
              paste(item_names,  paste(input[[items]], collapse = "/"))
          
          }
          )
      
          new_entry_df <- tibble::tibble(key = items,
                                   values = unlist(all_items)) %>% 
              tidyr::pivot_wider(tidyselect::everything(),
                                 names_from = "key",
                                 values_from = "values") %>% 
              dplyr::mutate(person_id_conferences = usr$person_id) 
          
          DBI::dbAppendTable(ipcas_db, "conferences", new_entry_df)
          

      if (input$conference_location == "Domácí") {
        
      section_iii_conference$domestic[[
        length(
          section_iii_conference$domestic)+1]] <- paste(c(all_items,"<br>"), collapse = "<br>")
      
      ids_domestic <-  ipcas_db %>%
          dplyr::tbl("conferences") %>%
          dplyr::filter(person_id_conferences == !!usr$person_id) %>% 
          dplyr::filter(conference_location == "Domácí") %>% 
          dplyr::pull(conference_id)
      
      
      updateSelectInput(session = session,
                        "remove_list_domestic",
                        choices = stats::setNames(
                            ids_domestic,
                            seq_along(ids_domestic)))
      
      
      } else {
        
        section_iii_conference$foreign[[as.character(
          length(
              section_iii_conference$foreign)+1)
        ]] <- paste(c(all_items,"<br>"), collapse = "<br>")
        
        
        ids_foreign <- ipcas_db %>%
            dplyr::tbl("conferences") %>%
            dplyr::filter(person_id_conferences == !!usr$person_id) %>% 
            dplyr::filter(conference_location == "Zahraniční") %>% 
            dplyr::pull(conference_id)
        
        updateSelectInput(session = session,
                          "remove_list_foreign",
                          choices = stats::setNames(
                              ids_foreign,
                              seq_along(ids_foreign)))
        
        
      }
      
   
      
    })
    
    observeEvent(input$remove_domestic, {
      
      
      section_iii_conference$domestic[as.integer(input$remove_list_domestic)] <- NULL 
      
      
      pool::dbExecute(ipcas_db, 
                      "DELETE FROM postgrad WHERE conference_id IN (?)",
                      params = list(input$remove_list))
      
      ids_domestic <-  ipcas_db %>%
          dplyr::tbl("conferences") %>%
          dplyr::filter(person_id_conferences == !!usr$person_id) %>% 
          dplyr::filter(conference_location == "Domácí") %>% 
          dplyr::pull(conference_id)
      
      
      updateSelectInput(session = session,
                        "remove_list_domestic",
                        choices = stats::setNames(
                            ids_domestic,
                            seq_along(ids_domestic)))

      
    })
    
    observeEvent(input$remove_foreign, {
      
     
      section_iii_conference$foreign[as.integer(input$remove_list_foreign)] <- NULL 
      pool::dbExecute(ipcas_db, 
                      "DELETE FROM postgrad WHERE conference_id IN (?)",
                      params = list(input$remove_list))
      
      ids_foreign <- ipcas_db %>%
          dplyr::tbl("conferences") %>%
          dplyr::filter(person_id_conferences == !!usr$person_id) %>% 
          dplyr::filter(conference_location == "Zahraniční") %>% 
          dplyr::pull(conference_id)
      
      updateSelectInput(session = session,
                        "remove_list_foreign",
                        choices = stats::setNames(
                            ids_foreign,
                            seq_along(ids_foreign)))
      
    })
    
    output$section_iii_conferences_domestic <- renderText({
      if (!is.null(section_iii_conference$domestic)) {
        paste(paste0(seq_along(section_iii_conference$domestic), ".<br>"),
              section_iii_conference$domestic)
      } else {""}
    })
    
    output$section_iii_conferences_foreign <- renderText({
      if (!is.null(section_iii_conference$foreign)) {
        paste(paste0(seq_along(section_iii_conference$foreign), ".<br>"),
              section_iii_conference$foreign)
      } else {""}
    })
    
    
    return(section_iii_conference)
    
    
  }
  )
}

 
