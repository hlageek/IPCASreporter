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
                               label = "Zadat do výkazu",                  icon = icon("check"),                  class = "btn-success"
                  )
  ),
  
  column(width = 6,
         
         
         h3("2)	Příspěvky a přednášky na konferencích:"),
         h4("a) Zahraniční:"),
         
         htmlOutput(ns("section_iii_conferences_foreign"), inline = FALSE),
         
         selectInput(ns("remove_list_foreign"), 
                     label = "Položka",
                     choices = ""),
         actionButton(ns("remove_foreign"),
                      label = "Odstranit z výkazu", class = "btn-primary", icon = icon("trash")
         ),
         
         h4("b) Domácí:"),
         htmlOutput(ns("section_iii_conferences_domestic"), inline = FALSE),
         
         selectInput(ns("remove_list_domestic"), 
                     label = "Položka",
                     choices = ""),
         actionButton(ns("remove_domestic"),
                      label = "Odstranit z výkazu", class = "btn-primary", icon = icon("trash")
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
    
    
    loc$names <- tibble::tibble(key = items,
                                names = item_names)
    
    
    # init ####
    observeEvent(usr$person_id, {

       

           loc$domestic <- transform_table(ipcas_db = ipcas_db,
                                           person_id = usr$person_id,
                                           tbl = "conferences",
                                           tbl_id = "conference_id",
                                           filter_col = "conference_location",
                                           filter_val = "Domácí",
                                           names_df = loc$names)
           
           loc$foreign <- transform_table(ipcas_db = ipcas_db,
                                          person_id = usr$person_id,
                                          tbl = "conferences",
                                          tbl_id = "conference_id",
                                          filter_col = "conference_location",
                                          filter_val = "Zahraniční",
                                          names_df = loc$names)
           
      
           ids_domestic <- loc$domestic %>% 
               dplyr::pull(conference_id)
           
           ids_foreign <- loc$foreign %>% 
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
        
        section_iii_conference$domestic <- paste0("<br>", 
                                                  loc$domestic$data,
                                                  "<br>")
        section_iii_conference$foreign <- paste0("<br>", 
                                                 loc$foreign$data,
                                                 "<br>")

    })
    
    
    # add ####
    observeEvent(input$add, {
     
          all_items <- purrr::map_chr(items, 
                                       .f = function(items) {
              
              unlist(paste(input[[items]], collapse = "/"))
          
          }
          )
      
          new_entry_df <- tibble::tibble(key = items,
                                   value = all_items) %>% 
              tidyr::pivot_wider(tidyselect::everything(),
                                 names_from = "key",
                                 values_from = "value") %>% 
              dplyr::mutate(person_id_conferences = usr$person_id) 
          
          DBI::dbAppendTable(ipcas_db, "conferences", new_entry_df)
         

      if (input$conference_location == "Domácí") {
          
          
          loc$domestic <- transform_table(ipcas_db = ipcas_db,
                                          person_id = usr$person_id,
                                          tbl = "conferences",
                                          tbl_id = "conference_id",
                                          filter_col = "conference_location",
                                          filter_val = "Domácí",
                                          names_df = loc$names)
          
      ids_domestic <-  loc$domestic %>% 
          dplyr::pull(conference_id)
      
      section_iii_conference$domestic <- paste0("<br>", 
                                                loc$domestic$data,
                                                "<br>")
      updateSelectInput(session = session,
                        "remove_list_domestic",
                        choices = stats::setNames(
                            ids_domestic,
                            seq_along(ids_domestic)))
      
      
      } else {
        
          loc$foreign <- transform_table(ipcas_db = ipcas_db,
                                          person_id = usr$person_id,
                                          tbl = "conferences",
                                          tbl_id = "conference_id",
                                          filter_col = "conference_location",
                                          filter_val = "Zahraniční",
                                          names_df = loc$names)
        
        
        ids_foreign <- loc$foreign %>% 
            dplyr::pull(conference_id)
        
        section_iii_conference$foreign <- paste0("<br>", 
                                                 loc$foreign$data,
                                                 "<br>")
        
        updateSelectInput(session = session,
                          "remove_list_foreign",
                          choices = stats::setNames(
                              ids_foreign,
                              seq_along(ids_foreign)))
        
      }
    

    })
    
    # remove domestic ####
    observeEvent(input$remove_domestic, {
        
    
      loc$domestic <- loc$domestic %>% 
          dplyr::filter(!conference_id %in% req(input$remove_list_domestic))
      
      pool::dbExecute(ipcas_db, 
                      "DELETE FROM conferences WHERE conference_id IN (?)",
                      params = list(input$remove_list_domestic))
      
      ids_domestic <-   loc$domestic %>%
          dplyr::pull(conference_id)
      
      
      updateSelectInput(session = session,
                        "remove_list_domestic",
                        choices = stats::setNames(
                            ids_domestic,
                            seq_along(ids_domestic)))
      
      section_iii_conference$domestic <- paste0("<br>", 
                                                loc$domestic$data,
                                                "<br>")
      section_iii_conference$foreign <- paste0("<br>", 
                                               loc$foreign$data,
                                               "<br>")
      
    })
    
    # remove foreign ####
    observeEvent(input$remove_foreign, {
      
        
        
        loc$foreign <- loc$foreign %>% 
            dplyr::filter(!conference_id %in% req(input$remove_list_foreign))
        
        pool::dbExecute(ipcas_db, 
                        "DELETE FROM conferences WHERE conference_id IN (?)",
                        params = list(input$remove_list_foreign))
        
        ids_foreign <-   loc$foreign %>%
            dplyr::pull(conference_id)
        
      
      updateSelectInput(session = session,
                        "remove_list_foreign",
                        choices = stats::setNames(
                            ids_foreign,
                            seq_along(ids_foreign)))
      
      section_iii_conference$domestic <- paste0("<br>", 
                                                loc$domestic$data,
                                                "<br>")
      section_iii_conference$foreign <- paste0("<br>", 
                                               loc$foreign$data,
                                               "<br>")
      
    })
    
    # output domestic ####
    output$section_iii_conferences_domestic <- renderText({
       
        if ( nrow(loc$domestic)>0 ) {
            
            text_to_display <- loc$domestic %>% 
                dplyr::pull(data)
            
            paste0(
                paste0(seq_along(text_to_display), ".<br>"),
                text_to_display,
                "<br><br>")
        } 
        
        else {""}
 
    })
    
    # output foreign ####
    output$section_iii_conferences_foreign <- renderText({
    
        if ( nrow(loc$foreign)>0 ) {
            
            text_to_display <- loc$foreign %>% 
                dplyr::pull(data)
            
            paste0(
                paste0(seq_along(text_to_display), ".<br>"),
                text_to_display,
                "<br><br>")
        } 
     
          else {""}
 
    })
    
    # return value ####
    return(section_iii_conference)
    
    
  }
  )
}

 
