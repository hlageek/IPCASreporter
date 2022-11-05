#' av21 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_av21_ui <- function(id, i18n){
  ns <- NS(id)
  
  fluidRow(column(width = 6,
                  
    
      textInput(ns("av21_program"), label = "Program Strategie AV21"),
      textInput(ns("av21_activity"), label = "Název aktivity (projektu)"),
      textInput(ns("av21_person"), label = "Řešitel aktivity (projektu)"),
      textAreaInput(ns("av21_annotation_cze"), label = "Anotace česky",
                    placeholder = "Lze zkopírovat z návrhového listu aktivity"),
      textAreaInput(ns("av21_annotation_eng"), label = "Anotace anglicky"),
      textAreaInput(ns("av21_results"), label = "Výstupy (včetně příp. odkazu na ASEP)"),

      textInput(ns("av21_partner"), label = "Spolupracující instituce"),


      actionButton(ns("add"),
                   label = "Zadat do výkazu",                  icon = icon("check"),                  class = "btn-success"
      )
    
  ),
  
  column(width = 6,
         
         htmlOutput(ns("section_v"), inline = FALSE),
         
         selectInput(ns("remove_list"), 
                     label = "Položka",
                     choices = ""),
         actionButton(ns("remove"),
                      label = "Odstranit z výkazu", class = "btn-primary", icon = icon("trash")
         )
         
         
  )
  )
}
    
#' av21 Server Function
#'
#' @noRd 
mod_av21_server <- function(id, usr, i18n) {
  moduleServer(id, function(input, output, session) {
    
    section_v <- reactiveValues()
    loc <- reactiveValues()
    
    items <- c(
      "av21_program",
      "av21_activity",
      "av21_person",
      "av21_annotation_cze",
      "av21_annotation_eng",
      "av21_results",
      "av21_partner"
    )
    
    loc$item_names <- 
        c(
      "Program Strategie AV21:",
      "Název aktivity (projektu)",
      "Řešitel aktivity (projektu):",
      "Anotace česky:",
      "Anotace anglicky:",
      "Výstupy (včetně příp. odkazu na ASEP):",
      "Spolupracující instituce:"
    )
    
    # init ####
    observeEvent(usr$person_id, {
      
        loc$names <- tibble::tibble(key = items,
                                    names = loc$item_names)
        
        loc$av21 <- transform_table(ipcas_db = ipcas_db,
                                      person_id = usr$person_id,
                                      tbl = "av21",
                                      tbl_id = "av21_id",
                                      filter_col = NULL,
                                      filter_val = NULL,
                                      names_df = loc$names)
        
        ids_av21 <- loc$av21 %>% 
            dplyr::pull(av21_id)
        
        section_v$av21 <- paste0("<br>", 
                                 loc$av21$data,
                                 "<br>")
        
        updateSelectInput(session = session,
                          "remove_list",
                          choices = stats::setNames(
                              ids_av21,
                              seq_along(ids_av21)))
    })
        
    
    # add ####
    
    observeEvent(input$add, {
       
        # check and require inputs
        checks <- stats::setNames(loc$item_names, items)
        check_inputs(input, checks, text = "Zadejte")

        all_items <- collect_items(items, input)
        
        new_entry_df <- prep_new_entry(
            items, 
            all_items, 
            "av21", 
            usr$person_id, 
            as.integer( format(Sys.Date(), "%Y"))
            )
        
        DBI::dbAppendTable(ipcas_db, "av21", new_entry_df)
        
        loc$av21 <-  transform_table(ipcas_db = ipcas_db,
                                     person_id = usr$person_id,
                                     tbl = "av21",
                                     tbl_id = "av21_id",
                                     filter_col = NULL,
                                     filter_val = NULL,
                                     names_df = loc$names)
        ids_av21 <- loc$av21 %>% 
            dplyr::pull(av21_id)
        
        section_v$av21 <- paste0("<br>", 
                                 loc$av21$data,
                                 "<br>")
        
        updateSelectInput(session = session,
                          "remove_list",
                          choices = stats::setNames(
                              ids_av21,
                              seq_along(ids_av21)))

    })
    
    # remove av21 ####
    
    observeEvent(input$remove, {
        
        loc$av21 <- loc$av21 %>% 
            dplyr::filter(!av21_id %in% req(input$remove_list))
        
        
        pool::dbExecute(ipcas_db, 
                        "DELETE FROM av21 WHERE av21_id IN (?)",
                        params = list(input$remove_list))
        
        ids_av21 <- loc$av21 %>% 
            dplyr::pull(av21_id)
        
        section_v$av21 <- paste0("<br>", 
                                 loc$av21$data,
                                 "<br>")
        
        updateSelectInput(session = session,
                          "remove_list",
                          choices = stats::setNames(
                              ids_av21,
                              seq_along(ids_av21)))
        
    })
    
    # output av21 ####
    
    output$section_v <- renderText({
        if (nrow(loc$av21)>0) {
            
            text_to_display <- loc$av21 %>% 
                dplyr::pull(data)
            
            paste0(
                paste0(seq_along(text_to_display), ".<br>"),
                text_to_display,
                "<br><br>")
            
        } else {""}
    })
        
    return(section_v)
    
  })}
 

    

 
