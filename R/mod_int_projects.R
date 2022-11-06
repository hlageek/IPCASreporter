#' int_projects UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_int_projects_ui <- function(id, i18n){
  ns <- NS(id)
  
  fluidRow(column(width = 6,
    
    textInput(ns("int_projects_name"), label = i18n$t("Název projektu"),
              placeholder = "např. INTER-EXCELLENCE"),

    actionButton(ns("add"),
                 label = i18n$t("Zadat do výkazu"),                  icon = icon("check"),                  class = "btn-success"
    )
    
    ),
    
    column(width = 6,    
 
           htmlOutput(ns("section_viii_int_projects"), inline = FALSE),
           
           selectInput(ns("remove_list"), 
                       label = i18n$t("Položka"),
                       choices = ""),
           actionButton(ns("remove"),
                        label = i18n$t("Odstranit z výkazu"), class = "btn-primary", icon = icon("trash")
           )
           
           
    )
  )
}
    
#' int_projects Server Function
#'
#' @noRd 
mod_int_projects_server <- function(id, usr, i18n) {
  moduleServer(id, function(input, output, session) {
    
    
    section_viii_int_projects <- reactiveValues()
    loc <- reactiveValues()
    
    items <- c(
      "int_projects_name"
    )
    
    loc$item_names <- c(
      "Název projektu:"
    )
    
    
    # init ####
    observeEvent(usr$person_id, {
        
        loc$names <- tibble::tibble(key = items,
                                    names = loc$item_names)
        
        loc$int_projects <- transform_table(ipcas_db = ipcas_db,
                                       person_id = usr$person_id,
                                       tbl = "int_projects",
                                       tbl_id = "int_projects_id",
                                       filter_col = NULL,
                                       filter_val = NULL,
                                       names_df = loc$names)
        
        # updates after action
        section_viii_int_projects$projects <- paste0("<br>", 
                                            loc$int_projects$data,
                                            "<br>")
        ids_int_projects <- loc$int_projects %>% 
            dplyr::pull(int_projects_id)
        updateSelectInput(session = session,
                          "remove_list",
                          choices = stats::setNames(
                              ids_int_projects,
                              seq_along(ids_int_projects)))
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
            tbl = "int_projects", 
            person_id = usr$person_id, 
            year = as.integer( format(Sys.Date(), "%Y"))
        )
        
        DBI::dbAppendTable(ipcas_db, "int_projects", new_entry_df)
        
        loc$int_projects <-  transform_table(ipcas_db = ipcas_db,
                                        person_id = usr$person_id,
                                        tbl = "int_projects",
                                        tbl_id = "int_projects_id",
                                        filter_col = NULL,
                                        filter_val = NULL,
                                        names_df = loc$names)
        
        # updates after action
        section_viii_int_projects$projects <- paste0("<br>", 
                                            loc$int_projects$data,
                                            "<br>")
        ids_int_projects <- loc$int_projects %>% 
            dplyr::pull(int_projects_id)
        updateSelectInput(session = session,
                          "remove_list",
                          choices = stats::setNames(
                              ids_int_projects,
                              seq_along(ids_int_projects)))
        
    })
    
    # remove  ####
    
    observeEvent(input$remove, {
        
        loc$int_projects <- loc$int_projects %>% 
            dplyr::filter(!int_projects_id %in% req(input$remove_list))
        
        
        pool::dbExecute(ipcas_db, 
                        "DELETE FROM int_projects WHERE int_projects_id IN (?)",
                        params = list(input$remove_list))
        
        # updates after action
        section_viii_int_projects$projects <- paste0("<br>", 
                                            loc$int_projects$data,
                                            "<br>")
        ids_int_projects <- loc$int_projects %>% 
            dplyr::pull(int_projects_id)
        updateSelectInput(session = session,
                          "remove_list",
                          choices = stats::setNames(
                              ids_int_projects,
                              seq_along(ids_int_projects)))
        
    })
    
    # output int_projects ####
    
    output$section_viii_int_projects <- renderText({
        if (nrow(loc$int_projects)>0) {
            
            text_to_display <- loc$int_projects %>% 
                dplyr::pull(data)
            
            paste0(
                paste0(seq_along(text_to_display), ".<br>"),
                text_to_display,
                "<br><br>")
            
        } else {""}
    })
    
    return(section_viii_int_projects)
    
  })}

    

 
