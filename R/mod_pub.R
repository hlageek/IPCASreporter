#' pub UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_pub_ui <- function(id, i18n){
  ns <- NS(id)
  
  fluidRow(column(width = 4,
  
    
   uiOutput(ns("pubs")),
   
   
   actionButton(ns("add"),
                label = i18n$t("Zadat do výkazu"),   
                icon = icon("check"),     
                class = "btn-success"
   )
    
  ),
  
  column(width = 8,
         
         h4(i18n$t("Publikace zařazené do výkazu")),
         htmlOutput(ns("section_i"), inline = FALSE),
         
         
         )
  )
}
    
#' pub Server Function
#'
#' @noRd 
mod_pub_server <-  function(id, identification, usr, i18n) {
  moduleServer(id, function(input, output, session) {

      ns <- NS(id)
      section_i <- reactiveValues()
      year <-  as.integer( format(Sys.Date(), "%Y"))
      
      observeEvent(usr$person_id, {
          
          section_i$publist <- get_asep_sourced_data(ipcas_db = ipcas_db, 
                                                     tbl = "pubs", 
                                                     person_id = usr$person_id, 
                                                     year = year, 
                                                     col_target = "pub") 

          
      })
      
  
    
    output$pubs <- renderUI({

      if (!isTruthy(identification$employee_name)) {
        
          i18n()$t("Nejprve vyplňte osobní údaje.")
        
      } else {
        
       citations <-  get_asep(identification$employee_name, type = "pubs")
       
       if (!is.null(citations)) {
       
       displayed_citations <- purrr::map(
         citations,
         ~stringr::str_replace_all(.x, "<.*?>", " ")
          )
       
       
       tagList(

        checkboxGroupInput(ns("publist"), 
                           label = i18n()$t("Nedávné záznamy nalezené v ASEP."), 
                           width = "100%",
                           choiceNames = displayed_citations,
                           choiceValues = citations)

       )
      } else {
        
        paste0(i18n()$t("V ASEP nebyly nalezeny žádné záznamy pro autora"), " ", 
               identification$employee_name, 
               " ", i18n()$t("v roce"), " ",
               format(Sys.Date(), "%Y"), 
               " ", i18n()$t("nebo"), " ", 
               format(Sys.Date()-365, "%Y"), 
               ".")
      }
        
      }
      
      
    })
    
    
    observeEvent(input$add, {
    
        pub_ids <- get_asep_sourced_data(ipcas_db = ipcas_db, 
                                         tbl = "pubs", 
                                         person_id = usr$person_id, 
                                         year = year, 
                                         col_target = "pub_id") 
        
        if (length(pub_ids)>0) {
        pool::dbExecute(ipcas_db, 
                        "DELETE FROM pubs WHERE pub_id IN (?)",
                        params = list(pub_ids)
                        )
        }
        
        new_entry_df <- tibble::tibble(
            person_id_pubs = usr$person_id,
            pub_id_year = as.integer( format(Sys.Date(), "%Y")),
            pub = input$publist
        )
        
        if (exists("pub", new_entry_df)) {
            
        DBI::dbAppendTable(ipcas_db, "pubs", new_entry_df)
        }
  
        
        section_i$publist <-  get_asep_sourced_data(ipcas_db = ipcas_db, 
                                                    tbl = "pubs", 
                                                    person_id = usr$person_id, 
                                                    year = year, 
                                                    col_target = "pub") 
    
      })
   
    output$section_i <- renderText({
        paste(section_i$publist, collapse = "<br><br>")
    })
    
    return(section_i)
    
  })
  
}
    

 
