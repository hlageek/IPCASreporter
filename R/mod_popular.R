#' popular UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_popular_ui <- function(id, i18n){
  ns <- NS(id)
  
  fluidRow(column(width = 6,
                  
    

    textInput(ns("popular_contribution"), label = i18n$t("Název akce") ),
    textInput(ns("popular_description"), label = i18n$t("Popis aktivity") ),
    textInput(ns("popular_organizer_primary"), label = i18n$t("Hlavní pořadatel") ),
    textInput(ns("popular_organizer_secondary"), label = i18n$t("Spolupořadatel") ),
    textInput(ns("popular_place"), label = i18n$t("Místo konání akce") ),
    dateInput(ns("popular_date"), label = i18n$t("Datum konání akce") ),
    
    actionButton(ns("add"),
                 label = i18n$t("Zadat do výkazu") ,                  icon = icon("check"),                  class = "btn-success"
    )
    

  ),
  
  column(width = 6,
          
          htmlOutput(ns("section_vi_popular"), inline = FALSE),
          
          selectInput(ns("remove_list"), 
                      label = i18n$t("Položka") ,
                      choices = ""),
          actionButton(ns("remove"),
                       label = i18n$t("Odstranit z výkazu") , class = "btn-primary", icon = icon("trash")
          )
          
          
  )
  )
}
    
#' popular Server Function
#'
#' @noRd 
mod_popular_server <- function(id, usr, i18n) {
  moduleServer(id, function(input, output, session) {
    
    
    section_vi_popular <- reactiveValues()
    loc <- reactiveValues()
    
    items <- c(
      "popular_contribution",
      "popular_description",
      "popular_organizer_primary",
      "popular_organizer_secondary",
      "popular_place",
      "popular_date"
      )
    
    loc$item_names <- c(
      "Název akce:",
      "Popis aktivity:",
      "Hlavní pořadatel:",
      "Spolupořadatel:",
      "Místo konání akce:",
      "Datum konání akce:"
    )
    
    # init ####
    observeEvent(usr$person_id, {

        loc$names <- tibble::tibble(key = items,
                                    names = loc$item_names)
        
        loc$popular <- transform_table(ipcas_db = ipcas_db,
                                    person_id = usr$person_id,
                                    tbl = "popular",
                                    tbl_id = "popular_id",
                                    filter_col = NULL,
                                    filter_val = NULL,
                                    names_df = loc$names)
        
        # updates after action
        section_vi_popular$events <- paste0("<br>", 
                                 loc$popular$data,
                                 "<br>")
        ids_popular <- loc$popular %>% 
            dplyr::pull(popular_id)
        updateSelectInput(session = session,
                          "remove_list",
                          choices = stats::setNames(
                              ids_popular,
                              seq_along(ids_popular)))
    })
   
    
    # add ####
    
    observeEvent(input$add, {
        
        # check and require inputs
        checks <- stats::setNames(loc$item_names, items)
        check_inputs(input, checks, text = i18n()$t("Zadejte") , exclude = "popular_organizer_secondary")
        
        all_items <- collect_items(items, input)
        
        new_entry_df <- prep_new_entry(
            items, 
            all_items, 
            tbl = "popular", 
            person_id = usr$person_id, 
            year = as.integer( format(Sys.Date(), "%Y"))
        )
        
        DBI::dbAppendTable(ipcas_db, "popular", new_entry_df)
        
        loc$popular <-  transform_table(ipcas_db = ipcas_db,
                                     person_id = usr$person_id,
                                     tbl = "popular",
                                     tbl_id = "popular_id",
                                     filter_col = NULL,
                                     filter_val = NULL,
                                     names_df = loc$names)
        
        # updates after action
        section_vi_popular$events <- paste0("<br>", 
                                            loc$popular$data,
                                            "<br>")
        ids_popular <- loc$popular %>% 
            dplyr::pull(popular_id)
        updateSelectInput(session = session,
                          "remove_list",
                          choices = stats::setNames(
                              ids_popular,
                              seq_along(ids_popular)))
        
    })
    
    # remove  ####
    
    observeEvent(input$remove, {
        
        loc$popular <- loc$popular %>% 
            dplyr::filter(!popular_id %in% req(input$remove_list))
        
        
        pool::dbExecute(ipcas_db, 
                        "DELETE FROM popular WHERE popular_id IN (?)",
                        params = list(input$remove_list))
        
        # updates after action
        section_vi_popular$events <- paste0("<br>", 
                                            loc$popular$data,
                                            "<br>")
        ids_popular <- loc$popular %>% 
            dplyr::pull(popular_id)
        updateSelectInput(session = session,
                          "remove_list",
                          choices = stats::setNames(
                              ids_popular,
                              seq_along(ids_popular)))
        
    })
    
    # output popular ####
    
    output$section_vi_popular <- renderText({
        if (nrow(loc$popular)>0) {
            
            text_to_display <- loc$popular %>% 
                dplyr::pull(data)
            
            paste0(
                paste0(seq_along(text_to_display), ".<br>"),
                text_to_display,
                "<br><br>")
            
        } else {""}
    })
    
    return(section_vi_popular)
   
  })}
    

