#' school UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_school_ui <- function(id, i18n){
  ns <- NS(id)
  fluidRow(column(width = 6,
                  
    
    textInput(ns("school_contribution"), label = "Název přednášky či specifikace jiného druhu akce"),
    textInput(ns("school_name"), label = "Pořadatel/škola"),
    textAreaInput(ns("school_description"), label = "Popis činnosti" ),
    
    actionButton(ns("add"),
                 label = "Zadat do výkazu",                  icon = icon("check"),                  class = "btn-success"
    )
    
    
  ),
  
  column(width = 6,

         htmlOutput(ns("section_vi_school"), inline = FALSE),
         
         selectInput(ns("remove_list"), 
                     label = "Položka",
                     choices = ""),
         actionButton(ns("remove"),
                      label = "Odstranit z výkazu", class = "btn-primary", icon = icon("trash")
         )
    
  )
 
  )
}
    
#' school Server Function
#'
#' @noRd 
mod_school_server <- function(id, usr, i18n) {
  moduleServer(id, function(input, output, session) {
    
    section_vi_school <- reactiveValues()
    loc <- reactiveValues()
    
    items <- c(
      "school_contribution",
      "school_name",
      "school_description"
    )
    
    loc$item_names <- c(
      "Název přednášky či specifikace jiného druhu akce:",
      "Pořadatel/škola:",
      "Popis činnosti:"
    )
    
    
    # init ####
    observeEvent(usr$person_id, {
        
        loc$names <- tibble::tibble(key = items,
                                    names = loc$item_names)
        
        loc$school <- transform_table(ipcas_db = ipcas_db,
                                       person_id = usr$person_id,
                                       tbl = "school",
                                       tbl_id = "school_id",
                                       filter_col = NULL,
                                       filter_val = NULL,
                                       names_df = loc$names)
        
        # updates after action
        section_vi_school$events <- paste0("<br>", 
                                            loc$school$data,
                                            "<br>")
        ids_school <- loc$school %>% 
            dplyr::pull(school_id)
        updateSelectInput(session = session,
                          "remove_list",
                          choices = stats::setNames(
                              ids_school,
                              seq_along(ids_school)))
    })
    
    
    # add ####
    
    observeEvent(input$add, {
        
        # check and require inputs
        checks <- stats::setNames(loc$item_names, items)
        check_inputs(input, checks, text = "Zadejte", exclude = "description")
        
        all_items <- collect_items(items, input)
        
        new_entry_df <- prep_new_entry(
            items, 
            all_items, 
            tbl = "school", 
            person_id = usr$person_id, 
            year = as.integer( format(Sys.Date(), "%Y"))
        )
        
        DBI::dbAppendTable(ipcas_db, "school", new_entry_df)
        
        loc$school <-  transform_table(ipcas_db = ipcas_db,
                                        person_id = usr$person_id,
                                        tbl = "school",
                                        tbl_id = "school_id",
                                        filter_col = NULL,
                                        filter_val = NULL,
                                        names_df = loc$names)
        
        # updates after action
        section_vi_school$events <- paste0("<br>", 
                                            loc$school$data,
                                            "<br>")
        ids_school <- loc$school %>% 
            dplyr::pull(school_id)
        updateSelectInput(session = session,
                          "remove_list",
                          choices = stats::setNames(
                              ids_school,
                              seq_along(ids_school)))
        
    })
    
    # remove  ####
    
    observeEvent(input$remove, {
        
        loc$school <- loc$school %>% 
            dplyr::filter(!school_id %in% req(input$remove_list))
        
        
        pool::dbExecute(ipcas_db, 
                        "DELETE FROM school WHERE school_id IN (?)",
                        params = list(input$remove_list))
        
        # updates after action
        section_vi_school$events <- paste0("<br>", 
                                            loc$school$data,
                                            "<br>")
        ids_school <- loc$school %>% 
            dplyr::pull(school_id)
        updateSelectInput(session = session,
                          "remove_list",
                          choices = stats::setNames(
                              ids_school,
                              seq_along(ids_school)))
        
    })
    
    # output school ####
    
    output$section_vi_school <- renderText({
        if (nrow(loc$school)>0) {
            
            text_to_display <- loc$school %>% 
                dplyr::pull(data)
            
            paste0(
                paste0(seq_along(text_to_display), ".<br>"),
                text_to_display,
                "<br><br>")
            
        } else {""}
    })
    
    return(section_vi_school)
    
  })}
    

 
