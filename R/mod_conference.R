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
                            label = i18n$t("Název příspěvku")),
                  
                  textInput(ns("conference_organizer"), 
                            label = i18n$t("Pořadatel")),
                  
                  textInput(ns("conference_name"), 
                            label = i18n$t("Název konference")),
                  
                  dateRangeInput(ns("conference_date"), 
                            label = i18n$t("Datum konání"),
                            language = "cs"),
                  
                  radioButtons(ns("conference_location"), 
                               label = i18n$t("Místo konání"), 
                               choices = c("Domácí" = "Domácí",
                                           "Zahraniční" = "Zahraniční")
                               ),
                  
                  

                  actionButton(ns("add"),
                               label = i18n$t("Zadat do výkazu"),                  icon = icon("check"),                  class = "btn-success"
                  )
  ),
  
  column(width = 6,
         
         
         h3(i18n$t("2) Příspěvky a přednášky na konferencích:")),
         h4(i18n$t("a) Zahraniční:")),
         
         htmlOutput(ns("section_iii_conferences_foreign"), inline = FALSE),
         
         selectInput(ns("remove_list_foreign"), 
                     label = i18n$t("Položka"),
                     choices = ""),
         actionButton(ns("remove_foreign"),
                      label = i18n$t("Odstranit z výkazu"), class = "btn-primary", icon = icon("trash")
         ),
         
         h4(i18n$t("b) Domácí:")),
         htmlOutput(ns("section_iii_conferences_domestic"), inline = FALSE),
         
         selectInput(ns("remove_list_domestic"), 
                     label = i18n$t("Položka"),
                     choices = ""),
         actionButton(ns("remove_domestic"),
                      label = i18n$t("Odstranit z výkazu"), class = "btn-primary", icon = icon("trash")
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
    
    
    names_df <- tibble::tibble(key = items,
                               names = item_names)
    
    # init ####
    observeEvent(usr$person_id, {

       

           loc$domestic <- transform_table(ipcas_db = ipcas_db,
                                           person_id = usr$person_id,
                                           tbl = "conferences",
                                           tbl_id = "conference_id",
                                           filter_col = "conference_location",
                                           filter_val = "Domácí",
                                           names_df = names_df)
           
           loc$foreign <- transform_table(ipcas_db = ipcas_db,
                                          person_id = usr$person_id,
                                          tbl = "conferences",
                                          tbl_id = "conference_id",
                                          filter_col = "conference_location",
                                          filter_val = "Zahraniční",
                                          names_df = names_df)
           
      
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
     
        checks <- stats::setNames(item_names, items)
        check_inputs(input, checks, text = i18n()$t("Zadejte"))
        
          all_items <- collect_items(items, input)
      
          new_entry_df <- prep_new_entry(
              items, 
              all_items, 
              "conferences", 
              usr$person_id, 
              as.integer( format(Sys.Date(), "%Y")),
              col_prefix = "conference"
          )
          
          DBI::dbAppendTable(ipcas_db, "conferences", new_entry_df)
         

      if (input$conference_location == "Domácí") {
          
          
          loc$domestic <- transform_table(ipcas_db = ipcas_db,
                                          person_id = usr$person_id,
                                          tbl = "conferences",
                                          tbl_id = "conference_id",
                                          filter_col = "conference_location",
                                          filter_val = "Domácí",
                                          names_df = names_df)
          
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
                                          names_df = names_df)
        
        
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
    
    # translation #####
    
    observe({
    updateRadioButtons(session, 
                       "conference_location", 
                       label = i18n()$t("Místo konání"),
                       choiceNames =  c(i18n()$t("Domácí"),
                                        i18n()$t("Zahraniční")),
                       choiceValues = c("Domácí", "Zahraniční"),
                       selected = "Domácí"
    )
    })
    # return value ####
    return(section_iii_conference)
    
    
  }
  )
}

 
