#' other_member UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_other_member_ui <- function(id, i18n){
  ns <- NS(id)
  
  fluidRow(column(width = 6,
                  
                  textInput(ns("other_member_name"), 
                            label = i18n$t("Druh grémia")),
                  
                  textInput(ns("other_member_institute"), 
                            label = i18n$t("Organizace (instituce/periodikum/nakladatelství)")),
                  
                  textInput(ns("other_member_position"), 
                            label = i18n$t("Funkce a funkční období")),
                  
                  radioButtons(ns("other_member_location"), 
                               label = NULL,
                               choices = c("Domácí" = "Domácí",
                                           "Zahraniční" = "Zahraniční")),
                  
                  actionButton(ns("add"),
                               label = i18n$t("Zadat do výkazu"),
                               icon = icon("check"),
                               class = "btn-success"
                  )
                  
  ),
  
  column(width = 6, 
         
         h3(i18n$t("Domácí")),
         
         htmlOutput(ns("section_ix_member_domestic"), inline = FALSE),
         
         selectInput(ns("remove_list_domestic"), 
                     label = i18n$t("Položka"),
                     choices = ""),
         
         br(), br(),
         actionButton(ns("remove_domestic"),
                      label = i18n$t("Odstranit z výkazu"), class = "btn-primary", icon = icon("trash")
         ),
         
         h3(i18n$t("Zahraniční")),
         
         htmlOutput(ns("section_ix_member_foreign"), inline = FALSE),
         
         selectInput(ns("remove_list_foreign"), 
                     label = i18n$t("Položka"),
                     choices = ""),
         
         br(), br(),
         actionButton(ns("remove_foreign"),
                      label = i18n$t("Odstranit z výkazu"), class = "btn-primary", icon = icon("trash")
         )
         
         
         
  )
  
  )
  

}
    
#' other_member Server Function
#'
#' @noRd 
mod_other_member_server <- function(id, usr, i18n) {
  moduleServer(id, function(input, output, session) {
    
    
    section_ix_member <- reactiveValues()
    loc <- reactiveValues()
    
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
    
    
    
    names_df <- tibble::tibble(key = items,
                                names = item_names)
    
    # init ####
    observeEvent(usr$person_id, {
        
        loc$domestic <- transform_table(ipcas_db = ipcas_db,
                                      person_id = usr$person_id,
                                      tbl = "other_member",
                                      tbl_id = "other_member_id",
                                      filter_col = "other_member_location",
                                      filter_val = "Domácí",
                                      names_df = names_df)
        
        loc$foreign <- transform_table(ipcas_db = ipcas_db,
                                        person_id = usr$person_id,
                                        tbl = "other_member",
                                        tbl_id = "other_member_id",
                                        filter_col = "other_member_location",
                                        filter_val = "Zahraniční",
                                        names_df = names_df)
        
        
        section_ix_member$domestic <- paste0("<br>", 
                                             loc$domestic$data,
                                             "<br>")
        ids_domestic <-  loc$domestic %>% 
            dplyr::pull(other_member_id)
        updateSelectInput(session = session,
                          "remove_list_domestic",
                          choices = stats::setNames(
                              ids_domestic,
                              seq_along(ids_domestic)))
        
        
        section_ix_member$foreign <- paste0("<br>", 
                                            loc$foreign$data,
                                            "<br>")
        ids_foreign <-  loc$foreign %>% 
            dplyr::pull(other_member_id)
        updateSelectInput(session = session,
                          "remove_list_foreign",
                          choices = stats::setNames(
                              ids_foreign,
                              seq_along(ids_foreign)))
        
    })
    
    
    # add ####
    
    observeEvent(input$add, {
        
        # check and require inputs
        checks <- stats::setNames(item_names, items)
        check_inputs(input, checks, text = i18n()$t("Zadejte"))
        
        all_items <- collect_items(items, input)
        
        new_entry_df <- tibble::tibble(key = items,
                                       value = all_items) %>% 
            tidyr::pivot_wider(tidyselect::everything(),
                               names_from = "key",
                               values_from = "value") %>% 
            dplyr::mutate(person_id_other_member = usr$person_id,
                          other_member_id_year = as.integer( format(Sys.Date(), "%Y")),
                          other_member_location = input$other_member_location) 
        
        DBI::dbAppendTable(ipcas_db, "other_member", new_entry_df)
        
        if (input$other_member_location == "Domácí") {
            
            
            
            loc$domestic <- transform_table(ipcas_db = ipcas_db,
                                          person_id = usr$person_id,
                                          tbl = "other_member",
                                          tbl_id = "other_member_id",
                                          filter_col = "other_member_location",
                                          filter_val = "Domácí",
                                          names_df = names_df)
            
            
            section_ix_member$domestic <- paste0("<br>", 
                                                 loc$domestic$data,
                                                 "<br>")
            ids_domestic <-  loc$domestic %>% 
                dplyr::pull(other_member_id)
            updateSelectInput(session = session,
                              "remove_list_domestic",
                              choices = stats::setNames(
                                  ids_domestic,
                                  seq_along(ids_domestic)))
            
        } else {
            
            
            loc$foreign <- transform_table(ipcas_db = ipcas_db,
                                            person_id = usr$person_id,
                                            tbl = "other_member",
                                            tbl_id = "other_member_id",
                                            filter_col = "other_member_location",
                                            filter_val = "Zahraniční",
                                            names_df = names_df)
            
            
            section_ix_member$foreign <- paste0("<br>", 
                                                loc$foreign$data,
                                                "<br>")
            ids_foreign <-  loc$foreign %>% 
                dplyr::pull(other_member_id)
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
            dplyr::filter(!other_member_id %in% req(input$remove_list_domestic))
        
        pool::dbExecute(ipcas_db, 
                        "DELETE FROM other_member WHERE other_member_id IN (?)",
                        params = list(input$remove_list_domestic))
        
        section_ix_member$domestic <- paste0("<br>", 
                                             loc$domestic$data,
                                             "<br>")
        ids_domestic <-  loc$domestic %>% 
            dplyr::pull(other_member_id)
        updateSelectInput(session = session,
                          "remove_list_domestic",
                          choices = stats::setNames(
                              ids_domestic,
                              seq_along(ids_domestic)))
        
    })
    
    
    # remove foreign ####
    
    observeEvent(input$remove_foreign, {
        
        
        loc$foreign <- loc$foreign %>% 
            dplyr::filter(!other_member_id %in% req(input$remove_list_foreign))
        
        pool::dbExecute(ipcas_db, 
                        "DELETE FROM other_member WHERE other_member_id IN (?)",
                        params = list(input$remove_list_foreign))
        
        section_ix_member$foreign <- paste0("<br>", 
                                            loc$foreign$data,
                                            "<br>")
        ids_foreign <-  loc$foreign %>% 
            dplyr::pull(other_member_id)
        updateSelectInput(session = session,
                          "remove_list_foreign",
                          choices = stats::setNames(
                              ids_foreign,
                              seq_along(ids_foreign)))
        
    })
    
    # translation ####
    
    observe({
        updateRadioButtons(session, 
                           "other_member_location", 
                           label = NULL,
                           choiceNames =  c(i18n()$t("Domácí"),
                                            i18n()$t("Zahraniční")),
                           choiceValues = c("Domácí", "Zahraniční"),
                           selected = "Domácí"
        )
    })
    
    # output domestic ####
    
    output$section_ix_member_domestic <- renderText({
        if (nrow(loc$domestic)>0) {
            
            text_to_display <- loc$domestic %>% 
                dplyr::pull(data)
            
            paste0(
                paste0(seq_along(text_to_display), ".<br>"),
                text_to_display,
                "<br><br>")
            
        } else {""}
    })
    
    # output foreign ####
    
    output$section_ix_member_foreign <- renderText({
        
        if (nrow(loc$foreign)>0) {
            
            text_to_display <- loc$foreign %>% 
                dplyr::pull(data)
            
            paste0(
                paste0(seq_along(text_to_display), ".<br>"),
                text_to_display,
                "<br><br>")
            
        } else {""}
    })
    
    return(section_ix_member)
    
  })}

 
