#' wip UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_wip_ui <- function(id, i18n){
  ns <- NS(id)
  fluidRow(column(width = 6,
    
    textAreaInput(ns("wip_description"), label = i18n$t("Popis")),
    
    
    actionButton(ns("add"),
                 label = i18n$t("Zadat do výkazu"),                  icon = icon("check"),                  class = "btn-success"
    )
    
  ),
  
  column(width = 6, 
         
         htmlOutput(ns("section_x"), inline = FALSE),
         
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
    
#' wip Server Function
#'
#' @noRd 
mod_wip_server <- function(id, usr, i18n) {
  moduleServer(id, function(input, output, session) {
    
    section_x <- reactiveValues()
    loc <- reactiveValues()
    
    items <- c(
      "wip_description"
    )
    
    item_names <- c(
      "Popis:"
    )
    
    names_df <- tibble::tibble(key = items,
                               names = item_names)
    
    # init ####
    observeEvent(usr$person_id, {
        
        
        
        loc$wip <- transform_table(ipcas_db = ipcas_db,
                                    person_id = usr$person_id,
                                    tbl = "wip",
                                    tbl_id = "wip_id",
                                    filter_col = NULL,
                                    filter_val = NULL,
                                    names_df = names_df %>% dplyr::mutate(names = i18n()$t(names)))
        
        ids_wip <- loc$wip %>% 
            dplyr::pull(wip_id)
        
        section_x$wip <- paste0("<br>", 
                                 loc$wip$data,
                                 "<br>")
        
        updateSelectInput(session = session,
                          "remove_list",
                          choices = stats::setNames(
                              ids_wip,
                              seq_along(ids_wip)))
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
            "wip", 
            usr$person_id, 
            as.integer( format(Sys.Date(), "%Y"))
        )
        
        DBI::dbAppendTable(ipcas_db, "wip", new_entry_df)
        
        loc$wip <-  transform_table(ipcas_db = ipcas_db,
                                     person_id = usr$person_id,
                                     tbl = "wip",
                                     tbl_id = "wip_id",
                                     filter_col = NULL,
                                     filter_val = NULL,
                                     names_df = names_df %>% dplyr::mutate(names = i18n()$t(names)))
        ids_wip <- loc$wip %>% 
            dplyr::pull(wip_id)
        
        section_x$wip <- paste0("<br>", 
                                 loc$wip$data,
                                 "<br>")
        
        updateSelectInput(session = session,
                          "remove_list",
                          choices = stats::setNames(
                              ids_wip,
                              seq_along(ids_wip)))
        
    })
    
    # remove wip ####
    
    observeEvent(input$remove, {
        
        loc$wip <- loc$wip %>% 
            dplyr::filter(!wip_id %in% req(input$remove_list))
        
        
        pool::dbExecute(ipcas_db, 
                        "DELETE FROM wip WHERE wip_id IN (?)",
                        params = list(input$remove_list))
        
        ids_wip <- loc$wip %>% 
            dplyr::pull(wip_id)
        
        section_x$wip <- paste0("<br>", 
                                 loc$wip$data,
                                 "<br>")
        
        updateSelectInput(session = session,
                          "remove_list",
                          choices = stats::setNames(
                              ids_wip,
                              seq_along(ids_wip)))
        
    })
    
    # output wip ####
    
    output$section_x <- renderText({
        if (nrow(loc$wip)>0) {
            
            text_to_display <- loc$wip %>% 
                dplyr::pull(data)
            
            paste0(
                paste0(seq_along(text_to_display), ".<br>"),
                text_to_display,
                "<br><br>")
            
        } else {""}
    })
    return(section_x)
    
  })}
    

 
