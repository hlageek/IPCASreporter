#' various UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_various_ui <- function(id, i18n){
  ns <- NS(id)
  fluidRow(column(width = 6,
                  
    
    textAreaInput(ns("various_description"), label = i18n$t("Popis")),
    
    
    actionButton(ns("add"),
                 label = i18n$t("Zadat do výkazu"),                  icon = icon("check"),                  class = "btn-success"
    )
    
  ),
  
  column(width = 6, 
         
         htmlOutput(ns("section_xi"), inline = FALSE),
         
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
    
#' various Server Function
#'
#' @noRd 
mod_various_server <- function(id, usr, i18n) {
  moduleServer(id, function(input, output, session) {
    
    
    section_xi <- reactiveValues()
    loc <- reactiveValues()
    
    items <- c(
      "various_description"
    )
    
    item_names <- c(
      "Popis:"
    )
    
    names_df <- tibble::tibble(key = items,
                               names = item_names)
    
    # init ####
    observeEvent(usr$person_id, {
        
        
        
        loc$various <- transform_table(ipcas_db = ipcas_db,
                                    person_id = usr$person_id,
                                    tbl = "various",
                                    tbl_id = "various_id",
                                    filter_col = NULL,
                                    filter_val = NULL,
                                    names_df = names_df)
        
        ids_various <- loc$various %>% 
            dplyr::pull(various_id)
        
        section_xi$various <- paste0("<br>", 
                                     sanitize_output(
                                 loc$various$data
                                 ),
                                 "<br>")
        
        updateSelectInput(session = session,
                          "remove_list",
                          choices = stats::setNames(
                              ids_various,
                              seq_along(ids_various)))
    })
    
    
    # add ####
    
    observeEvent(input$add, {
        
        # check and require inputs
        checks <- stats::setNames(item_names, items)
        check_inputs(input, checks, text = i18n()$t("Zadejte"))
        
        all_items <- collect_items(items, input)
        
        new_entry_df <- prep_new_entry(
            items, 
            all_items, 
            "various", 
            usr$person_id, 
            as.integer( format(Sys.Date(), "%Y"))
        )
        
        DBI::dbAppendTable(ipcas_db, "various", new_entry_df)
        
        loc$various <-  transform_table(ipcas_db = ipcas_db,
                                     person_id = usr$person_id,
                                     tbl = "various",
                                     tbl_id = "various_id",
                                     filter_col = NULL,
                                     filter_val = NULL,
                                     names_df = names_df)
        ids_various <- loc$various %>% 
            dplyr::pull(various_id)
        
        section_xi$various <- paste0("<br>", 
                                     sanitize_output(
                                         loc$various$data
                                         ),
                                 "<br>")
        
        updateSelectInput(session = session,
                          "remove_list",
                          choices = stats::setNames(
                              ids_various,
                              seq_along(ids_various)))
        
    })
    
    # remove various ####
    
    observeEvent(input$remove, {
        
        loc$various <- loc$various %>% 
            dplyr::filter(!various_id %in% req(input$remove_list))
        
        
        pool::dbExecute(ipcas_db, 
                        "DELETE FROM various WHERE various_id IN (?)",
                        params = list(input$remove_list))
        
        ids_various <- loc$various %>% 
            dplyr::pull(various_id)
        
        section_xi$various <- paste0("<br>", 
                                     sanitize_output(
                                 loc$various$data
                                 ),
                                 "<br>")
        
        updateSelectInput(session = session,
                          "remove_list",
                          choices = stats::setNames(
                              ids_various,
                              seq_along(ids_various)))
        
    })
    
    # output various ####
    
    output$section_xi <- renderText({
        if (nrow(loc$various)>0) {
            
            text_to_display <- sanitize_output(loc$various %>% 
                dplyr::pull(data))
            
            paste0(
                paste0(seq_along(text_to_display), ".<br>"),
                text_to_display,
                "<br><br>")
            
        } else {""}
    })
    
    return(section_xi)
    
  })}
    

