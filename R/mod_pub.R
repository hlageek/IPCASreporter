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
      
      observeEvent(usr$person_id, {
          
          section_i$publist <- ipcas_db %>% 
              dplyr::tbl("pubs") %>% 
              dplyr::filter(person_id_pubs == !!usr$person_id) %>% 
              dplyr::pull(pub)
          
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
                           label = i18n()$t("Nedávné publikace nalezené v ASEPu."), 
                           width = "100%",
                           choiceNames = displayed_citations,
                           choiceValues = citations)

       )
      } else {
        
        paste0(i18n()$t("V ASEP nebyly nalezeny žádné záznamy pro autora "), 
               identification$employee_name, 
               i18n()$t(" v roce"), 
               format(Sys.Date(), "%Y"), 
               i18n()$t(" nebo "), 
               format(Sys.Date()-365, "%Y"), 
               ".")
      }
        
      }
      
      
    })
    
    
    observeEvent(input$add, {
    
      
        pub_ids <- ipcas_db %>% 
            dplyr::tbl("pubs") %>% 
            dplyr::filter(person_id_pubs == !!usr$person_id) %>% 
            dplyr::pull(pub_id)
        
        if (length(pub_ids)>0) {
        pool::dbExecute(ipcas_db, 
                        "DELETE FROM pubs WHERE pub_id IN (?)",
                        params = list(pub_ids)
                        )
        }
        
        purrr::walk(input$publist, .f = function(x) {
        pool::dbExecute(ipcas_db, 
                        paste0( "INSERT INTO pubs",
                                " (",
                                "person_id_pubs,", 
                                "pub",
                                ")",
                                " VALUES(",
                                "'", usr$person_id, "',",
                                "'", x, "'",
                                ")"
                        )
        )
        })
        
        section_i$publist <- ipcas_db %>% 
            dplyr::tbl("pubs") %>% 
            dplyr::filter(person_id_pubs == !!usr$person_id) %>% 
            dplyr::pull(pub)
    
      })
   
    output$section_i <- renderText({
        paste(section_i$publist, collapse = "<br><br>")
    })
    
    return(section_i)
    
  })
  
}
    

 
