#' other_award UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_other_award_ui <- function(id, i18n){
  ns <- NS(id)
  fluidRow(column(width = 6,
    
    textAreaInput(ns("other_awards_description"), 
                  label = i18n$t("Ocenění")),
    p(i18n$t("Uveďte název ocenění a kým bylo uděleno.")),
    
    actionButton(ns("add"),
                 label = i18n$t("Zadat do výkazu"),                  icon = icon("check"),                  class = "btn-success"
    )
    
  ),
  
  column(width = 6, 
    
         htmlOutput(ns("section_ix_award"), inline = FALSE),
         
         selectInput(ns("remove_list"), 
                     label = i18n$t("Položka"),
                     choices = ""),
         actionButton(ns("remove"),
                      label = i18n$t("Odstranit z výkazu"), class = "btn-primary", icon = icon("trash")
         )
         
         
  )
 
  )
}
    
#' other_award Server Function
#'
#' @noRd 
mod_other_award_server <- function(id, usr, i18n) {
  moduleServer(id, function(input, output, session) {
    
    
      
      section_ix_award <- reactiveValues()
    loc <- reactiveValues()
    
    items <- c(
      "other_awards_description"
    )
    
    loc$item_names <- c(
      "Název ocenění:"
    )
    
    
    # init ####
    observeEvent(usr$person_id, {
        
        loc$names <- tibble::tibble(key = items,
                                    names = loc$item_names)
        
        loc$award <- transform_table(ipcas_db = ipcas_db,
                                       person_id = usr$person_id,
                                       tbl = "other_awards",
                                       tbl_id = "other_awards_id",
                                       filter_col = NULL,
                                       filter_val = NULL,
                                       names_df = loc$names )
        
        # updates after action
        section_ix_award$award <- paste0("<br>", 
                                         sanitize_output(
                                             loc$award$data
                                             ),
                                            "<br>")
        ids_award <- loc$award %>% 
            dplyr::pull(other_awards_id)
        updateSelectInput(session = session,
                          "remove_list",
                          choices = stats::setNames(
                              ids_award,
                              seq_along(ids_award)))
    })
    
    
    # add ####
    
    observeEvent(input$add, {
        
        # check and require inputs
        checks <- stats::setNames(loc$item_names, items)
        check_inputs(input, checks, text = i18n()$t("Zadejte"), exclude = NULL)
        
        all_items <- collect_items(items, input)
        
        new_entry_df <- prep_new_entry(
            items, 
            all_items, 
            tbl = "other_awards", 
            person_id = usr$person_id, 
            year = as.integer( format(Sys.Date(), "%Y"))
        )
        
        DBI::dbAppendTable(ipcas_db, "other_awards", new_entry_df)
        
        loc$award <-  transform_table(ipcas_db = ipcas_db,
                                        person_id = usr$person_id,
                                        tbl = "other_awards",
                                        tbl_id = "other_awards_id",
                                        filter_col = NULL,
                                        filter_val = NULL,
                                        names_df = loc$names)
        
        # updates after action
        section_ix_award$award <- paste0("<br>", 
                                         sanitize_output(
                                             loc$award$data
                                             ),
                                            "<br>")
        ids_award <- loc$award %>% 
            dplyr::pull(other_awards_id)
        updateSelectInput(session = session,
                          "remove_list",
                          choices = stats::setNames(
                              ids_award,
                              seq_along(ids_award)))
        
    })
    
    # remove  ####
    
    observeEvent(input$remove, {
        
        loc$award <- loc$award %>% 
            dplyr::filter(!other_awards_id %in% req(input$remove_list))
        
        
        pool::dbExecute(ipcas_db, 
                        "DELETE FROM other_awards WHERE other_awards_id IN (?)",
                        params = list(input$remove_list))
        
        # updates after action
        section_ix_award$award <- paste0("<br>", 
                                         sanitize_output(
                                             loc$award$data
                                             ),
                                            "<br>")
        ids_award <- loc$award %>% 
            dplyr::pull(other_awards_id)
        updateSelectInput(session = session,
                          "remove_list",
                          choices = stats::setNames(
                              ids_award,
                              seq_along(ids_award)))
        
    })
    
    # output award ####
    
    output$section_ix_award <- renderText({
        if (nrow(loc$award)>0) {
            
            text_to_display <- sanitize_output(
                loc$award %>% 
                dplyr::pull(data)
            )
            
            paste0(
                paste0(seq_along(text_to_display), ".<br>"),
                text_to_display,
                "<br><br>")
            
        } else {""}
    })
    
    
    return(section_ix_award)
    
  })}
    

 
