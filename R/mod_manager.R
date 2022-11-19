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
                   
                   selectInput(ns("persons"),
                               label = i18n$t("Osoby"),
                               selected = "",
                               choices = "",
                               multiple = TRUE
                   ),
                   
                   actionButton(ns("test"), "test"),
                   
                   htmlOutput(ns("res"))
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
 
    loc <- reactiveValues()
    
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
    
    observeEvent(req(input$department), {
        
        loc$dpt_people <- dplyr::tbl(ipcas_db, "departments") %>% 
            dplyr::filter(department == !!input$department) %>% 
            dplyr::pull(person_id_departments)
        
        loc$people <- dplyr::tbl(ipcas_db, "persons") %>% 
            dplyr::filter(person_id %in% !!loc$dpt_people) %>% 
            dplyr::arrange(name_last) %>% 
            dplyr::collect()
        
        updateSelectInput(session = session,
                          "persons",
                          selected = loc$people$person_id,
                          choices = stats::setNames(
                              loc$people$person_id,
                              paste(loc$people$name_first, loc$people$name_last))
                          )
    })
    
    observeEvent(input$test, {browser()})
    observeEvent({req(input$department)
                 req(input$persons)}, {
        
       # browser()
       
        
        
        
        domestic_people <- dplyr::tbl(ipcas_db, "conferences") %>% 
            dplyr::filter(person_id_conferences %in% !!loc$dpt_people) %>% 
            dplyr::filter(person_id_conferences %in% !!input$persons) %>% 
            dplyr::select("person_id_conferences", "conference_id") %>% 
            dplyr::collect()
        
        domestic <- transform_table(ipcas_db = ipcas_db,
                                        person_id = loc$dpt_people,
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

         
        domestic_df <- domestic_people %>% 
            dplyr::left_join(domestic, by = "conference_id") %>% 
            dplyr::inner_join(loc$people, by = c("person_id_conferences" = "person_id")) %>% 
            dplyr::bind_rows(
                loc$people %>% 
                    dplyr::filter(person_id %in% !!input$persons) %>% 
                    dplyr::rename("person_id_conferences" = "person_id") %>% 
                    dplyr::anti_join(domestic_people, by = "person_id_conferences")
                             ) %>% 
            dplyr::arrange(name_last)  %>%
            tidyr::unite("name", c(name_first, name_last), sep = " ") %>% 
            dplyr::mutate(name = paste0("<h5>", name, "</h5>")) %>% 
            dplyr::mutate(dplyr::across(where(is.character), tidyr::replace_na, "" ))

            
        # tbl_names <- DBI::dbListTables(ipcas_db)
        # tbl_names <- tbl_names[!grepl("persons", tbl_names)]
        # 
        # df_collection <- purrr::map2(tbl_names, dpt_people, 
        #                              .f = function(x,y) {
        #    dplyr::tbl(ipcas_db, x) %>% 
        #         dplyr::filter(.data[[paste0("person_id_", x)]] %in% y) %>% 
        #         dplyr::collect() %>% 
        #         dplyr::mutate(person_id = y)
        #})
    
             
        output$res <- renderText({ 
            paste(domestic_df$name,
                  sanitize_output(domestic_df$data), sep = "")
            })

    })
    
  })
}
    
## To be copied in the UI
# mod_manager_ui("manager_1")

## To be copied in the server
# mod_manager_server("manager_1")
