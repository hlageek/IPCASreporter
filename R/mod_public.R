#' public UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_public_ui <- function(id, i18n){
  ns <- NS(id)
  fluidRow(column(width = 6,
                  
    textInput(ns("gov_body"), label = "Instituce státní nebo veřejné správy"),
    textAreaInput(ns("gov_description"), label = "Popis spolupráce"),
    
    actionButton(ns("add"),
                 label = "Zadat do výkazu",                  icon = icon("check"),                  class = "btn-success"
    )
    
    
  ),
  
  column(width = 6,
    
         
         htmlOutput(ns("section_vii"), inline = FALSE),
         
         selectInput(ns("remove_list"), 
                     label = "Položka",
                     choices = ""),
         actionButton(ns("remove"),
                      label = "Odstranit z výkazu", class = "btn-primary", icon = icon("trash")
         )
         
  )
  )
}
    
#' public Server Function
#'
#' @noRd 
mod_public_server <- function(id, usr, i18n) {
  moduleServer(id, function(input, output, session) {
    
    section_vii <- reactiveValues()
    loc <- reactiveValues()
    
    items <- c(
      "gov_body",
      "gov_description"
    )
    
    loc$item_names <- c(
      "Instituce státní nebo veřejné správy:",
      "Popis spolupráce:"
    )
    
    
    # init ####
    observeEvent(usr$person_id, {
        
        loc$names <- tibble::tibble(key = items,
                                    names = loc$item_names)
        
        loc$gov <- transform_table(ipcas_db = ipcas_db,
                                       person_id = usr$person_id,
                                       tbl = "gov",
                                       tbl_id = "gov_id",
                                       filter_col = NULL,
                                       filter_val = NULL,
                                       names_df = loc$names)
        
        # updates after action
        section_vii$public <- paste0("<br>", 
                                            loc$gov$data,
                                            "<br>")
        ids_gov <- loc$gov %>% 
            dplyr::pull(gov_id)
        updateSelectInput(session = session,
                          "remove_list",
                          choices = stats::setNames(
                              ids_gov,
                              seq_along(ids_gov)))
    })
    
    
    # add ####
    
    observeEvent(input$add, {
        
        # check and require inputs
        checks <- stats::setNames(loc$item_names, items)
        check_inputs(input, checks, text = "Zadejte", exclude = NULL)
        
        all_items <- collect_items(items, input)
        
        new_entry_df <- prep_new_entry(
            items, 
            all_items, 
            tbl = "gov", 
            person_id = usr$person_id, 
            year = as.integer( format(Sys.Date(), "%Y"))
        )
        
        DBI::dbAppendTable(ipcas_db, "gov", new_entry_df)
        
        loc$gov <-  transform_table(ipcas_db = ipcas_db,
                                        person_id = usr$person_id,
                                        tbl = "gov",
                                        tbl_id = "gov_id",
                                        filter_col = NULL,
                                        filter_val = NULL,
                                        names_df = loc$names)
        
        # updates after action
        section_vii$public <- paste0("<br>", 
                                            loc$gov$data,
                                            "<br>")
        ids_gov <- loc$gov %>% 
            dplyr::pull(gov_id)
        updateSelectInput(session = session,
                          "remove_list",
                          choices = stats::setNames(
                              ids_gov,
                              seq_along(ids_gov)))
        
    })
    
    # remove  ####
    
    observeEvent(input$remove, {
        
        loc$gov <- loc$gov %>% 
            dplyr::filter(!gov_id %in% req(input$remove_list))
        
        
        pool::dbExecute(ipcas_db, 
                        "DELETE FROM gov WHERE gov_id IN (?)",
                        params = list(input$remove_list))
        
        # updates after action
        section_vii$public <- paste0("<br>", 
                                            loc$gov$data,
                                            "<br>")
        ids_gov <- loc$gov %>% 
            dplyr::pull(gov_id)
        updateSelectInput(session = session,
                          "remove_list",
                          choices = stats::setNames(
                              ids_gov,
                              seq_along(ids_gov)))
        
    })
    
    # output gov ####
    
    output$section_vii <- renderText({
        if (nrow(loc$gov)>0) {
            
            text_to_display <- loc$gov %>% 
                dplyr::pull(data)
            
            paste0(
                paste0(seq_along(text_to_display), ".<br>"),
                text_to_display,
                "<br><br>")
            
        } else {""}
    })
    
    
    return(section_vii)
    
  })}
    

