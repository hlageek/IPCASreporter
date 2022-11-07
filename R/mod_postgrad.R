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
                              label = i18n$t("Název VŠ:"),
                              choices = NULL,
                              selected = ""),
                  
                  selectInput(ns("postgrad_faculty"), 
                              label = i18n$t("Název fakulty:"),
                              choices = NULL,
                              selected = ""),
                  selectInput(ns("postgrad_program"), 
                              label = i18n$t("Název studijního programu/studijního oboru:"),
                              choices = NULL,
                              selected = ""),
                  
                  selectInput(ns("postgrad_year"), 
                              label = i18n$t("Akademický rok, semestr:"),
                              choices = c(paste0(format(Sys.Date(), "%Y"), ", LS"),
                                          paste0(format(Sys.Date(), "%Y"), ", ZS"))),
                  
                  radioButtons(ns("postgrad_level"), 
                               label = "", 
                               choices = c("Doktorský studijní program"), 
                               inline = TRUE),
                  
                  textInput(ns("postgrad_course"), 
                            label = i18n$t("Název předmětu:")),
                  
                  
                  postgrad_types(id), tags$br(),
                  
                  
                  numericInput(ns("postgrad_hours"), 
                               label = i18n$t("Počet odučených hodin:"), 
                               value = 0,
                               min = 0,
                               step = 1),
                  
                  textAreaInput(ns("postgrad_other"), 
                                label = i18n$t("Jiné")),
                  
                  actionButton(ns("add"),
                               label = i18n$t("Zadat do výkazu"),                  icon = icon("check"),                  class = "btn-success"
                  )
                  
  ),
  
  column(width = 6,
         
         
         h3(i18n$t("1)	Výuka na vysokých školách a vedení prací:")),
         h4(i18n$t("b) Doktorský studijní program")),
         
         htmlOutput(ns("section_iii_postgrad_preview"), inline = FALSE),
         
         selectInput(ns("remove_list"), 
                     label = i18n$t("Položka"),
                     choices = ""),
         actionButton(ns("remove"),
                      label = i18n$t("Odstranit z výkazu"), class = "btn-primary", icon = icon("trash")
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

  names_df <- tibble::tibble(key = items,
                             names = item_names)
  #  on startup ####

  observeEvent(usr$person_id, {

      uni_choices <- IPCASreporter::universities %>%
          dplyr::pull(university)
      uni_choices <- unique(uni_choices)

      updateSelectInput(session = session,
                        "postgrad_school",
                        choices =  c("", sort(uni_choices))
      )



loc$all_df <- transform_table(
    ipcas_db = ipcas_db, 
    tbl = "postgrad", 
    tbl_id = "postgrad_id", 
    person_id = usr$person_id,
    names_df = names_df)
    
    
    section_iii_postgrad$data <- paste0("<br>",  as.list(loc$all_df$data), "<br>")
    
    updateSelectInput(session = session,
                      "remove_list",
                      choices = stats::setNames(loc$all_df$postgrad_id, seq_along(loc$all_df$postgrad_id)))
  })
  
  
  # add ####
  observeEvent(input$add, {
      checks <- stats::setNames(item_names, items)
      check_inputs(input, checks, text = i18n()$t("Zadejte"), exclude = "other|faculty|program")

    all_items <- collect_items(items, input)

    
    new_df <- prep_new_entry(
        items, 
        all_items, 
        "postgrad", 
        usr$person_id, 
        as.integer( format(Sys.Date(), "%Y"))
    )
    
    DBI::dbAppendTable(ipcas_db, "postgrad", new_df)
    
    
    loc$all_df <- transform_table(
        ipcas_db = ipcas_db, 
        tbl = "postgrad", 
        tbl_id = "postgrad_id", 
        person_id = usr$person_id,
        names_df = names_df)

    
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
      
      loc$all_df <-  transform_table(
          ipcas_db = ipcas_db, 
          tbl = "postgrad", 
          tbl_id = "postgrad_id", 
          person_id = usr$person_id,
          names_df = names_df)
      
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
  
  # translations ####
  
  observe({
      updateRadioButtons(session, 
                         "postgrad_level", 
                         label = i18n()$t("Typ studijního programu/oboru:"),
                         choiceNames = i18n$t("Doktorský studijní program"),
                         choiceValues = "Doktorský studijní program"
      )
      updateRadioButtons(session, 
                         "postgrad_type_prednasky", 
                         label = i18n()$t("Přednášky:"),
                         choiceNames = c(i18n()$t("ano"), 
                                         i18n()$t("ne")),
                         choiceValues = c("ano", "ne"),
                         inline = TRUE
      )
      
      
  })
  
  # preview ####
  
  output$section_iii_postgrad_preview <- renderText({
      if (nrow(loc$all_df)>0) {
          paste(paste0("<br>", seq_along(section_iii_postgrad$data), "."),
                section_iii_postgrad$data)
      } else {""}
  })
  
  
  return(section_iii_postgrad)
  
  })
}
