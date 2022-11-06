#' other_editions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_other_editions_ui <- function(id, i18n){
  ns <- NS(id)
  
  fluidRow(column(width = 6,
                  
                  textInput(ns("other_editions_name"), 
                            label = i18n$t("Název díla")),
                  
                  textAreaInput(ns("other_editions_description"), 
                            label = i18n$t("Doplňující informace")),
                  
                  actionButton(ns("add"),
                               label = i18n$t("Zadat do výkazu"),                  icon = icon("check"),                  class = "btn-success"
                  )
                  
  ),
  
  column(width = 6, 
         

         htmlOutput(ns("section_ix_editions"), inline = FALSE),
         
         selectInput(ns("remove_list"), 
                     label = i18n$t("Položka"),
                     choices = ""),
         
         br(), br(),
         actionButton(ns("remove"),
                      label = i18n$t("Odstranit z výkazu"), class = "btn-primary", icon = icon("trash")
         )
         
         
  )
  
  )
  

}
    
#' other_editions Server Function
#'
#' @noRd 
mod_other_editions_server <- function(id, usr, i18n) {
  moduleServer(id, function(input, output, session) {
    
    
    section_ix_editions <- reactiveValues()
    loc <- reactiveValues()
    
    items <- c(
      "other_editions_name",
      "other_editions_description"
      )
    
    item_names <- c(
      "Název:",
      "Doplňující informace:"
    )
    
    
    names_df <- tibble::tibble(key = items,
                               names = item_names)
    
    # init ####
    observeEvent(usr$person_id, {
        
        
        
        loc$other_editions <- transform_table(ipcas_db = ipcas_db,
                                    person_id = usr$person_id,
                                    tbl = "other_editions",
                                    tbl_id = "other_editions_id",
                                    filter_col = NULL,
                                    filter_val = NULL,
                                    names_df = names_df %>% dplyr::mutate(names = i18n()$t(names)))
        
        ids_other_editions <- loc$other_editions %>% 
            dplyr::pull(other_editions_id)
        
        section_ix_editions$editions  <- paste0("<br>", 
                                 loc$other_editions$data,
                                 "<br>")
        
        updateSelectInput(session = session,
                          "remove_list",
                          choices = stats::setNames(
                              ids_other_editions,
                              seq_along(ids_other_editions)))
    })
    
    
    # add ####
    
    observeEvent(input$add, {
        
        # check and require inputs
        checks <- stats::setNames(item_names, items)
        check_inputs(input, checks, text = i18n()$t("Zadejte"), exclude = "description")
        
        all_items <- collect_items(items, input)
        
        new_entry_df <- prep_new_entry(
            items, 
            all_items, 
            "other_editions", 
            usr$person_id, 
            as.integer( format(Sys.Date(), "%Y"))
        )
        
        DBI::dbAppendTable(ipcas_db, "other_editions", new_entry_df)
        
        loc$other_editions <-  transform_table(ipcas_db = ipcas_db,
                                     person_id = usr$person_id,
                                     tbl = "other_editions",
                                     tbl_id = "other_editions_id",
                                     filter_col = NULL,
                                     filter_val = NULL,
                                     names_df = names_df %>% dplyr::mutate(names = i18n()$t(names)))
        ids_other_editions <- loc$other_editions %>% 
            dplyr::pull(other_editions_id)
        
        section_ix_editions$editions  <- paste0("<br>", 
                                 loc$other_editions$data,
                                 "<br>")
        
        updateSelectInput(session = session,
                          "remove_list",
                          choices = stats::setNames(
                              ids_other_editions,
                              seq_along(ids_other_editions)))
        
    })
    
    # remove other_editions ####
    
    observeEvent(input$remove, {
        
        loc$other_editions <- loc$other_editions %>% 
            dplyr::filter(!other_editions_id %in% req(input$remove_list))
        
        
        pool::dbExecute(ipcas_db, 
                        "DELETE FROM other_editions WHERE other_editions_id IN (?)",
                        params = list(input$remove_list))
        
        ids_other_editions <- loc$other_editions %>% 
            dplyr::pull(other_editions_id)
        
        section_ix_editions$editions  <- paste0("<br>", 
                                 loc$other_editions$data,
                                 "<br>")
        
        updateSelectInput(session = session,
                          "remove_list",
                          choices = stats::setNames(
                              ids_other_editions,
                              seq_along(ids_other_editions)))
        
    })
    
    # output other_editions ####
    
    output$section_ix_editions <- renderText({
        if (nrow(loc$other_editions)>0) {
            
            text_to_display <- loc$other_editions %>% 
                dplyr::pull(data)
            
            paste0(
                paste0(seq_along(text_to_display), ".<br>"),
                text_to_display,
                "<br><br>")
            
        } else {""}
    })
    return(section_ix_editions)
    
  })}

 
