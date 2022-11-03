#' postgrad UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_postgrad_ui <- function(id, i18n){
  ns <- NS(id)
  fluidRow(column(width = 6,
                  
                  selectInput(ns("postgrad_school"),
                              label = "Název VŠ:",
                              choices = NULL,
                              selected = ""),
                  
                  selectInput(ns("postgrad_faculty"), 
                              label = "Název fakulty:",
                              choices = NULL,
                              selected = ""),
                  selectInput(ns("postgrad_program"), 
                              label = "Název studijního programu/studijního oboru:",
                              choices = NULL,
                              selected = ""),
                  
                  selectInput(ns("postgrad_year"), 
                              label = "Akademický rok, semestr:",
                              choices = c(paste0(format(Sys.Date(), "%Y"), ", LS"),
                                          paste0(format(Sys.Date(), "%Y"), ", ZS"))),
                  
                  radioButtons(ns("postgrad_level"), 
                               label = "", 
                               choices = c("Doktorský studijní program"), 
                               inline = TRUE),
                  
                  textInput(ns("postgrad_course"), 
                            label = "Název předmětu:"),
                  
                  
                  postgrad_types(id), tags$br(),
                  
                  
                  numericInput(ns("postgrad_hours"), 
                               label = "Počet odučených hodin:", 
                               value = 0,
                               min = 0,
                               step = 1),
                  
                  textAreaInput(ns("postgrad_other"), 
                                label = "Jiné"),
                  
                  actionButton(ns("add"),
                               label = "Add to report",                  icon = icon("check"),                  class = "btn-success"
                  )
                  
  ),
  
  column(width = 6,
         
         
         h3("1)	Výuka na vysokých školách a vedení prací:"),
         h4("b) Doktorský studijní program"),
         
         htmlOutput(ns("section_iii_postgrad_preview"), inline = FALSE),
         
         selectInput(ns("remove_list"), 
                     label = "Item",
                     choices = ""),
         actionButton(ns("remove"),
                      label = "Remove from report", class = "btn-primary", icon = icon("trash")
         )
  )
  )
}
    
#' postgrad Server Function
#'
#' @noRd 
mod_postgrad_server <- function(id, usr, i18n) {
  moduleServer(id, function(input, output, session) {
  
  loc <- reactiveValues()
  section_iii_postgrad <- reactiveValues()
  
  # names of things ####
  
  items <- c(
    "postgrad_school",
    "postgrad_faculty",
    "postgrad_program",
    "postgrad_year",
    "postgrad_level",
    "postgrad_course",
    "postgrad_type_prednasky",
    "postgrad_type_seminare",
    "postgrad_type_cviceni",
    "postgrad_type_vedeni",
    "postgrad_type_texty",
    "postgrad_hours",
    "postgrad_other"
  )
  
  item_names <- c(
    "Název VŠ:",
    "Název fakulty:",
    "Název studijního programu/oboru:",
    "Akademický rok, semestr:",
    "Typ studijního programu/oboru:",
    "Název předmětu:",
    "Přednášky:",
    "Semináře:",
    "Cvičení:",
    "Vedení dizertačních prací:",
    "Učební texty:",
    "Počet odučených hodin:",
    "Jiné:"
    
  )

  #  on startup ####

  observeEvent(usr$person_id, {

      uni_choices <- IPCASreporter::universities %>%
          dplyr::pull(university)
      uni_choices <- unique(uni_choices)

      updateSelectInput(session = session,
                        "postgrad_school",
                        choices =  c("", sort(uni_choices))
      )

      loc$names <- tibble::tibble(key = items,
                                  names = item_names)

loc$all_df <-  ipcas_db %>%
    dplyr::tbl("postgrad") %>%
    dplyr::filter(person_id_postgrad == !!usr$person_id) %>%
    dplyr::select(-person_id_postgrad) %>%
    tidyr::pivot_longer(-postgrad_id,
                        names_to = "key",
                        values_to = "value") %>%
    dplyr::collect() %>%
    dplyr::left_join(loc$names, by = "key") %>%
    tidyr::unite("value", c(names, value), sep = " ") %>%
    dplyr::select(-key) %>%
    dplyr::group_by(postgrad_id) %>%
    dplyr::summarise(data = stringr::str_flatten(value,                                                    collapse = "<br>"))
section_iii_postgrad$data <- paste0("<br>",  as.list(loc$all_df$data), "<br>")

      updateSelectInput(session = session,
                        "remove_list",
                        choices = stats::setNames(loc$all_df$postgrad_id, seq_along(loc$all_df$postgrad_id)))



  })

  # add ####

  observeEvent(input$add, {

    all_items <- list()

    for (i in seq_along(items)) {

        all_items <- c(all_items, input[[items[i]]])
        

    }
    new_df <- tibble::tibble(key = items,
                             values = unlist(all_items)) %>% 
        tidyr::pivot_wider(everything(),
                           names_from = "key",
                           values_from = "values") %>% 
        dplyr::mutate(person_id_postgrad = usr$person_id) 
    
    DBI::dbAppendTable(ipcas_db, "postgrad", new_df)
    
    
    loc$all_df <- ipcas_db %>% 
        dplyr::tbl("postgrad") %>% 
        dplyr::filter(person_id_postgrad == !!usr$person_id) %>%
        dplyr::select(-person_id_postgrad) %>% 
        tidyr::pivot_longer(-postgrad_id,
                            names_to = "key",
                            values_to = "value") %>%
        dplyr::collect() %>% 
        dplyr::left_join(loc$names, by = "key") %>% 
        tidyr::unite("value", c(names, value), sep = " ") %>% 
        dplyr::select(-key) %>% 
        dplyr::group_by(postgrad_id) %>% 
        dplyr::summarise(data = stringr::str_flatten(value,                                                    collapse = "<br>")) 
    
    
    
    
    section_iii_postgrad$data <- paste0("<br>",  as.list(loc$all_df$data), "<br>")
    
    
    updateSelectInput(session = session,
                      "remove_list", 
                      choices = stats::setNames(loc$all_df$postgrad_id, seq_along(loc$all_df$postgrad_id))
    )
    
    
    
    
  })
  
  # remove  ####
  
  observeEvent(input$remove, {
      
      
      
      pool::dbExecute(ipcas_db, 
                      "DELETE FROM postgrad WHERE postgrad_id IN (?)",
                      params = list(input$remove_list))
      
      loc$all_df <-  ipcas_db %>% 
          dplyr::tbl("postgrad") %>% 
          dplyr::filter(person_id_postgrad == !!usr$person_id) %>%
          dplyr::select(-person_id_postgrad) %>% 
          tidyr::pivot_longer(-postgrad_id,
                              names_to = "key",
                              values_to = "value") %>%
          dplyr::collect() %>% 
          dplyr::left_join(loc$names, by = "key") %>% 
          tidyr::unite("value", c(names, value), sep = " ") %>% 
          dplyr::select(-key) %>% 
          dplyr::group_by(postgrad_id) %>% 
          dplyr::summarise(data = stringr::str_flatten(value,                                                    collapse = "<br>")) 
      
      section_iii_postgrad$data <- paste0("<br>",  as.list(loc$all_df$data), "<br>")
      
      updateSelectInput(session = session,
                        "remove_list", 
                        choices = stats::setNames(loc$all_df$postgrad_id, seq_along(loc$all_df$postgrad_id))
                        
      )
      
  })
  
  # Update selection options based on choices ####
  
  observeEvent(input$postgrad_school, {
      
      
      
      choices_fac <- IPCASreporter::universities %>% 
          dplyr::filter(university == input$postgrad_school &
                            !is.na(faculty)) %>% 
          dplyr::pull(faculty) %>% 
          unique() %>% 
          sort()
      
      
      updateSelectInput(session = session,
                        "postgrad_faculty", 
                        choices = choices_fac
                        
      )
      
  })
  
  observeEvent(input$postgrad_faculty, {
      
      choices_prog_check <- IPCASreporter::universities %>% 
          dplyr::filter(university == input$postgrad_school) %>% 
          dplyr::pull(faculty) %>%
          unique()
      
      if (length(choices_prog_check)==1) {
          
          choices_prog <- IPCASreporter::universities %>% 
              dplyr::filter(university == input$postgrad_school &
                                !is.na(disc_program) &
                                stringr::str_detect(type, "bakalářský|magisterský") 
              ) %>% 
              dplyr::pull(disc_program) %>% 
              unique() %>% 
              sort()
          
      } else {
          
          choices_prog <- IPCASreporter::universities %>% 
              dplyr::filter(university == input$postgrad_school &
                                faculty == input$postgrad_faculty &
                                !is.na(disc_program) &
                                stringr::str_detect(type, "bakalářský|magisterský") 
              ) %>% 
              dplyr::pull(disc_program) %>% 
              unique() %>% 
              sort()
          
      }
      
      updateSelectInput(session = session,
                        "postgrad_program", 
                        choices = choices_prog)
      
  })
  
  
  # preview ####
  
  output$section_iii_postgrad_preview <- renderText({
      if (length(section_iii_postgrad$data)>0) {
          paste(paste0("<br>", seq_along(section_iii_postgrad$data), "."),
                section_iii_postgrad$data)
      } else {""}
  })
  
  
  return(section_iii_postgrad)
  
  })
}
