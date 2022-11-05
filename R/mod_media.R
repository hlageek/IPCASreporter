#' media UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_media_ui <- function(id, i18n){
  ns <- NS(id)
  fluidRow(column(width = 6,
                  
    
    textInput(ns("media_contribution"), label = "Název pořadu nebo textu"),
    textInput(ns("media_name"), label = "Médium"),
    textAreaInput(ns("media_description"), label = "Doplňující informace" ),
    
    actionButton(ns("add"),
                 label = "Zadat do výkazu",                  icon = icon("check"),                  class = "btn-success"
    )
    
    
  ),
  
  column(width = 6,

         htmlOutput(ns("section_vi_media"), inline = FALSE),
         
         selectInput(ns("remove_list"), 
                     label = "Položka",
                     choices = ""),
         actionButton(ns("remove"),
                      label = "Odstranit z výkazu", class = "btn-primary", icon = icon("trash")
         )
    
  )
 
  )
}
    
#' media Server Function
#'
#' @noRd 
mod_media_server <- function(id, usr, i18n) {
  moduleServer(id, function(input, output, session) {
    
    section_vi_media <- reactiveValues()
    loc <- reactiveValues()
    
    items <- c(
      "media_contribution",
      "media_name",
      "media_description"
    )
    
    loc$item_names <- c(
      "Název:",
      "Médium:",
      "Doplňující informace:"
    )
    
    
    # init ####
    observeEvent(usr$person_id, {
        
        loc$names <- tibble::tibble(key = items,
                                    names = loc$item_names)
        
        loc$media <- transform_table(ipcas_db = ipcas_db,
                                       person_id = usr$person_id,
                                       tbl = "media",
                                       tbl_id = "media_id",
                                       filter_col = NULL,
                                       filter_val = NULL,
                                       names_df = loc$names)
        
        # updates after action
        section_vi_media$media <- paste0("<br>", 
                                            loc$media$data,
                                            "<br>")
        ids_media <- loc$media %>% 
            dplyr::pull(media_id)
        updateSelectInput(session = session,
                          "remove_list",
                          choices = stats::setNames(
                              ids_media,
                              seq_along(ids_media)))
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
            tbl = "media", 
            person_id = usr$person_id, 
            year = as.integer( format(Sys.Date(), "%Y"))
        )
        
        DBI::dbAppendTable(ipcas_db, "media", new_entry_df)
        
        loc$media <-  transform_table(ipcas_db = ipcas_db,
                                        person_id = usr$person_id,
                                        tbl = "media",
                                        tbl_id = "media_id",
                                        filter_col = NULL,
                                        filter_val = NULL,
                                        names_df = loc$names)
        
        # updates after action
        section_vi_media$media <- paste0("<br>", 
                                            loc$media$data,
                                            "<br>")
        ids_media <- loc$media %>% 
            dplyr::pull(media_id)
        updateSelectInput(session = session,
                          "remove_list",
                          choices = stats::setNames(
                              ids_media,
                              seq_along(ids_media)))
        
    })
    
    # remove  ####
    
    observeEvent(input$remove, {
        
        loc$media <- loc$media %>% 
            dplyr::filter(!media_id %in% req(input$remove_list))
        
        
        pool::dbExecute(ipcas_db, 
                        "DELETE FROM media WHERE media_id IN (?)",
                        params = list(input$remove_list))
        
        # updates after action
        section_vi_media$media <- paste0("<br>", 
                                            loc$media$data,
                                            "<br>")
        ids_media <- loc$media %>% 
            dplyr::pull(media_id)
        updateSelectInput(session = session,
                          "remove_list",
                          choices = stats::setNames(
                              ids_media,
                              seq_along(ids_media)))
        
    })
    
    # output media ####
    
    output$section_vi_media <- renderText({
        if (nrow(loc$media)>0) {
            
            text_to_display <- loc$media %>% 
                dplyr::pull(data)
            
            paste0(
                paste0(seq_along(text_to_display), ".<br>"),
                text_to_display,
                "<br><br>")
            
        } else {""}
    })
    
    return(section_vi_media)
    
  })}
    

 
