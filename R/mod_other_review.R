#' other_review UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_other_review_ui <- function(id, i18n){
  ns <- NS(id)
  
  
  fluidRow(column(width = 6,
                  
                  textInput(ns("other_reviews_name"), 
                            label = i18n$t("Název")),
                  textAreaInput(ns("other_reviews_description"), 
                                label = i18n$t("Doplňující informace")),
                  
                  
                  actionButton(ns("add"),
                               label = i18n$t("Zadat do výkazu"),                  icon = icon("check"),                  class = "btn-success"
                  )
                  
  ),
  
  column(width = 6, 
         
         htmlOutput(ns("section_ix_review"), inline = FALSE),
         
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
    
#' other_review Server Function
#'
#' @noRd 
mod_other_review_server <- function(id, usr, i18n) {
  moduleServer(id, function(input, output, session) {
  
    section_ix_review <- reactiveValues()
    loc <- reactiveValues()
    
    items <- c(
      "other_reviews_name",
      "other_reviews_description"
    )
    
    item_names <- c(
      "Název:",
      "Doplňující informace:"
    )
    
    
    
    # init ####
    observeEvent(usr$person_id, {
        
        loc$names <- tibble::tibble(key = items,
                                    names = item_names)
        
        loc$review <- transform_table(ipcas_db = ipcas_db,
                                     person_id = usr$person_id,
                                     tbl = "other_reviews",
                                     tbl_id = "other_reviews_id",
                                     filter_col = NULL,
                                     filter_val = NULL,
                                     names_df = loc$names )
        
        # updates after action
        section_ix_review$review <- paste0("<br>", 
                                         loc$review$data,
                                         "<br>")
        ids_review <- loc$review %>% 
            dplyr::pull(other_reviews_id)
        updateSelectInput(session = session,
                          "remove_list",
                          choices = stats::setNames(
                              ids_review,
                              seq_along(ids_review)))
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
            tbl = "other_reviews", 
            person_id = usr$person_id, 
            year = as.integer( format(Sys.Date(), "%Y"))
        )
        
        DBI::dbAppendTable(ipcas_db, "other_reviews", new_entry_df)
        
        loc$review <-  transform_table(ipcas_db = ipcas_db,
                                      person_id = usr$person_id,
                                      tbl = "other_reviews",
                                      tbl_id = "other_reviews_id",
                                      filter_col = NULL,
                                      filter_val = NULL,
                                      names_df = loc$names)
        
        # updates after action
        section_ix_review$review <- paste0("<br>", 
                                         loc$review$data,
                                         "<br>")
        ids_review <- loc$review %>% 
            dplyr::pull(other_reviews_id)
        updateSelectInput(session = session,
                          "remove_list",
                          choices = stats::setNames(
                              ids_review,
                              seq_along(ids_review)))
        
    })
    
    # remove  ####
    
    observeEvent(input$remove, {
        
        loc$review <- loc$review %>% 
            dplyr::filter(!other_reviews_id %in% req(input$remove_list))
        
        
        pool::dbExecute(ipcas_db, 
                        "DELETE FROM other_reviews WHERE other_reviews_id IN (?)",
                        params = list(input$remove_list))
        
        # updates after action
        section_ix_review$review <- paste0("<br>", 
                                         loc$review$data,
                                         "<br>")
        ids_review <- loc$review %>% 
            dplyr::pull(other_reviews_id)
        updateSelectInput(session = session,
                          "remove_list",
                          choices = stats::setNames(
                              ids_review,
                              seq_along(ids_review)))
        
    })
    
    # output review ####
    
    output$section_ix_review <- renderText({
        if (nrow(loc$review)>0) {
            
            text_to_display <- loc$review %>% 
                dplyr::pull(data)
            
            paste0(
                paste0(seq_along(text_to_display), ".<br>"),
                text_to_display,
                "<br><br>")
            
        } else {""}
    })
    
    
    return(section_ix_review)
    
  })}

 
