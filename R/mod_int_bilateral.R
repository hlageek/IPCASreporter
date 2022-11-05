#' int_bilateral UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_int_bilateral_ui <- function(id, i18n){
  ns <- NS(id)
  
  fluidRow(column(width = 6,
                  
                  textInput(ns("int_bilateral_description"), label = "Bilaterální spolupráce"),
                  
                  actionButton(ns("add"),
                               label = "Add to report",                  icon = icon("check"),                  class = "btn-success"
                  )
                  
  ),
  
  column(width = 6,    
         
         htmlOutput(ns("section_viii_int_bilateral"), inline = FALSE),
         
         selectInput(ns("remove_list"), 
                     label = "Item",
                     choices = ""),
         actionButton(ns("remove"),
                      label = "Remove from report", class = "btn-primary", icon = icon("trash")
         )
         
         
  )
  )

}
    
#' int_bilateral Server Function
#'
#' @noRd 
mod_int_bilateral_server <- function(id, usr, i18n) {
  moduleServer(id, function(input, output, session) {
    
    
    section_viii_int_bilateral <- reactiveValues()
    loc <- reactiveValues()
    
    items <- c(
      "int_bilateral_description"
    )
    
    loc$item_names <- c(
      "Bilaterální spolupráce:"
    )
    
    
    # init ####
    observeEvent(usr$person_id, {
        
        loc$names <- tibble::tibble(key = items,
                                    names = loc$item_names)
        
        loc$int_bilateral <- transform_table(ipcas_db = ipcas_db,
                                            person_id = usr$person_id,
                                            tbl = "int_bilateral",
                                            tbl_id = "int_bilateral_id",
                                            filter_col = NULL,
                                            filter_val = NULL,
                                            names_df = loc$names)
        
        # updates after action
        section_viii_int_bilateral$bilateral <- paste0("<br>", 
                                                   loc$int_bilateral$data,
                                                   "<br>")
        ids_int_bilateral <- loc$int_bilateral %>% 
            dplyr::pull(int_bilateral_id)
        updateSelectInput(session = session,
                          "remove_list",
                          choices = stats::setNames(
                              ids_int_bilateral,
                              seq_along(ids_int_bilateral)))
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
            tbl = "int_bilateral", 
            person_id = usr$person_id, 
            year = as.integer( format(Sys.Date(), "%Y"))
        )
        
        DBI::dbAppendTable(ipcas_db, "int_bilateral", new_entry_df)
        
        loc$int_bilateral <-  transform_table(ipcas_db = ipcas_db,
                                             person_id = usr$person_id,
                                             tbl = "int_bilateral",
                                             tbl_id = "int_bilateral_id",
                                             filter_col = NULL,
                                             filter_val = NULL,
                                             names_df = loc$names)
        
        # updates after action
        section_viii_int_bilateral$bilateral <- paste0("<br>", 
                                                   loc$int_bilateral$data,
                                                   "<br>")
        ids_int_bilateral <- loc$int_bilateral %>% 
            dplyr::pull(int_bilateral_id)
        updateSelectInput(session = session,
                          "remove_list",
                          choices = stats::setNames(
                              ids_int_bilateral,
                              seq_along(ids_int_bilateral)))
        
    })
    
    # remove  ####
    
    observeEvent(input$remove, {
        
        loc$int_bilateral <- loc$int_bilateral %>% 
            dplyr::filter(!int_bilateral_id %in% req(input$remove_list))
        
        
        pool::dbExecute(ipcas_db, 
                        "DELETE FROM int_bilateral WHERE int_bilateral_id IN (?)",
                        params = list(input$remove_list))
        
        # updates after action
        section_viii_int_bilateral$bilateral <- paste0("<br>", 
                                                   loc$int_bilateral$data,
                                                   "<br>")
        ids_int_bilateral <- loc$int_bilateral %>% 
            dplyr::pull(int_bilateral_id)
        updateSelectInput(session = session,
                          "remove_list",
                          choices = stats::setNames(
                              ids_int_bilateral,
                              seq_along(ids_int_bilateral)))
        
    })
    
    # output int_bilateral ####
    
    output$section_viii_int_bilateral <- renderText({
        if (nrow(loc$int_bilateral)>0) {
            
            text_to_display <- loc$int_bilateral %>% 
                dplyr::pull(data)
            
            paste0(
                paste0(seq_along(text_to_display), ".<br>"),
                text_to_display,
                "<br><br>")
            
        } else {""}
    })
    
    return(section_viii_int_bilateral)
    
  })}

    

 
