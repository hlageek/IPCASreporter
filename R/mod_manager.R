#' manager UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_manager_ui <- function(id, i18n){
  ns <- NS(id)

      tabsetPanel(
          tabPanel("Osobní náhled",
                   preview_standard(ns, i18n)
                   ),
          tabPanel("Náhled oddělení",
                   
                   selectInput(ns("department"),
                               label = i18n$t("Oddělení"),
                               selected = "",
                               choices = c("", IPCASreporter::departments$department_name),
                               multiple = FALSE
                   ),
                   
                   actionButton(ns("test"), "test"),
                   
                   dataTableOutput(ns("res"))
                   )
      )
  
}
    
#' manager Server Functions
#'
#' @noRd 
mod_manager_server <- function(id,
                               identification,
                               section_i,
                               section_ii,
                               section_iii_undergrad,
                               section_iii_postgrad,
                               section_iii_conference,
                               section_iii_lecture,
                               section_iv,
                               section_v,
                               section_vi_popular,
                               section_vi_school,
                               section_vi_media,
                               section_vii,
                               section_viii_int_projects,
                               section_viii_int_bilateral,
                               section_ix_award,
                               section_ix_review,
                               section_ix_member,
                               section_ix_editions,
                               section_x,
                               section_xi,
                               usr){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    render_preview(output,
                   identification,
                   section_i,
                   section_ii,
                   section_iii_undergrad,
                   section_iii_postgrad,
                   section_iii_conference,
                   section_iii_lecture,
                   section_iv,
                   section_v,
                   section_vi_popular,
                   section_vi_school,
                   section_vi_media,
                   section_vii,
                   section_viii_int_projects,
                   section_viii_int_bilateral,
                   section_ix_award,
                   section_ix_review,
                   section_ix_member,
                   section_ix_editions,
                   section_x,
                   section_xi)
    
    observeEvent(input$test, {
        
        dpt_people <- dplyr::tbl(ipcas_db, "departments") %>% 
            dplyr::filter(department == !!input$department) %>% 
            dplyr::pull(person_id_departments)
        
        people <- dplyr::tbl(ipcas_db, "persons") %>% 
            dplyr::filter(person_id %in% dpt_people) %>% 
            dplyr::collect()
        
        domestic_people <- dplyr::tbl(ipcas_db, "conferences") %>% 
            dplyr::filter(person_id_conferences %in% dpt_people) %>% 
            dplyr::select("person_id_conferences", "conference_id") %>% 
            dplyr::collect()
        
        domestic <- transform_table(ipcas_db = ipcas_db,
                                        person_id = dpt_people,
                                        tbl = "conferences",
                                        tbl_id = "conference_id",
                                        filter_col = "conference_location",
                                        filter_val = "Domácí",
                                        names_df = tibble::tibble(        
                                            key = c(
                                            "conference_contribution",
                                            "conference_organizer",
                                            "conference_name",
                                            "conference_date",
                                            "conference_location"
                                        ),
                                            names = c(
                                            "Název příspěvku:",
                                            "Pořadatel:",
                                            "Název konference:",
                                            "Datum konání:",
                                            "Místo konání:"
                                        )))
        
        domestic_people %>% 
            dplyr::inner_join(domestic) %>% 
            dplyr::left_join(people, by = c("person_id_conferences" = "person_id")) %>% 
            tidyr::unite("name", c(name_first, name_last)) %>% 
            dplyr::mutate(name = paste0("<h5>", name, "</h5>"))

            
        tbl_names <- DBI::dbListTables(ipcas_db)
        tbl_names <- tbl_names[!grepl("persons", tbl_names)]

        df_collection <- purrr::map2(tbl_names, dpt_people, 
                                     .f = function(x,y) {
           dplyr::tbl(ipcas_db, x) %>% 
                dplyr::filter(.data[[paste0("person_id_", x)]] %in% y) %>% 
                dplyr::collect() %>% 
                dplyr::mutate(person_id = y)
        })
        
        browser()
        list_of_df <- df_collection %>% 
            purrr::map(~dplyr::left_join(.x, people,
                                         by = "person_id"))
             
        output$res <- renderDataTable(list_of_df[[1]])

    })
    
  })
}
    
## To be copied in the UI
# mod_manager_ui("manager_1")

## To be copied in the server
# mod_manager_server("manager_1")
