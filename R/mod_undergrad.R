#' undergrad UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_undergrad_ui <- function(id, i18n){
    ns <- NS(id)
    
    
    
    fluidRow(column(width = 6,
                    
                    selectInput(ns("undergrad_school"), 
                                label = i18n$t("Název VŠ:"), 
                                choices = NULL, 
                                selected = ""),
                    
                    selectInput(ns("undergrad_faculty"), 
                                label = i18n$t("Název fakulty:"),
                                choices = NULL,
                                selected = ""),
                    selectInput(ns("undergrad_program"), 
                                label = i18n$t("Název studijního programu/studijního oboru:"),
                                choices = NULL,
                                selected = ""),
                    
                    selectInput(ns("undergrad_year"), 
                                label = i18n$t("Akademický rok, semestr:"),
                                choices = c(paste0(format(Sys.Date(), "%Y"), ", LS"),
                                            paste0(format(Sys.Date(), "%Y"), ", ZS"))),
                    
                    radioButtons(ns("undergrad_level"), 
                                 label = "", 
                                 choices = c("Bakalářský studijní program", 
                                             "Magisterský studijní program"), 
                                 inline = TRUE),
                    
                    textInput(ns("undergrad_course"), 
                              label = i18n$t("Název předmětu:")),
                    
                    
                    undergrad_types(id), tags$br(),
                    
                    
                    numericInput(ns("undergrad_hours"), 
                                 label = i18n$t("Počet odučených hodin:"), 
                                 value = 0,
                                 min = 0,
                                 step = 1),
                    
                    textAreaInput(ns("undergrad_other"), 
                                  label = i18n$t("Jiné")),
                    
                    actionButton(ns("add"),
                                 label = i18n$t("Zadat do výkazu"),                  icon = icon("check"),                  class = "btn-success"
                    )
                    
    ),
    
    column(width = 6,
           
           
           h3(i18n$t("1) Výuka na vysokých školách a vedení prací:")),
           h4(i18n$t("a) Bakalářské a magisterské studijní programy")),
           
           htmlOutput(ns("section_iii_undergrad_preview"), inline = FALSE),
           
           selectInput(ns("remove_list"), 
                       label = i18n$t("Položka"),
                       choices = ""),
           actionButton(ns("remove"),
                        label = i18n$t("Odstranit z výkazu"), class = "btn-primary", icon = icon("trash")
           )
           
           
    )
    )
}

#' undergrad Server Function
#'
#' @noRd 
mod_undergrad_server <- function(id, usr, i18n) {
    moduleServer(id, function(input, output, session) {
        
        loc <- reactiveValues()
        section_iii_undergrad <- reactiveValues()
        
        
        
        # names of things ####
        items <- c(
            "undergrad_school",
            "undergrad_faculty",
            "undergrad_program",
            "undergrad_year",
            "undergrad_level",
            "undergrad_course",
            "undergrad_type_prednasky",
            "undergrad_type_seminare",
            "undergrad_type_cviceni",
            "undergrad_type_vedeni",
            "undergrad_type_texty",
            "undergrad_hours",
            "undergrad_other"
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
            "Vedení bakalářských a diplomových prací:",
            "Učební texty:",
            "Počet odučených hodin:",
            "Jiné:"
            
        )
        
        
        names_df <- names_df_switch("undergrad")
        
        #  on startup ####
        
        observeEvent(usr$person_id, {
            
            uni_choices <- IPCASreporter::universities %>% 
                dplyr::pull(university)
            uni_choices <- unique(uni_choices)
        
            updateSelectInput(session = session,
                              "undergrad_school", 
                              choices =  c("", sort(uni_choices))
                              )
            
            loc$all_df <- transform_table(
                ipcas_db = ipcas_db, 
                tbl = "undergrad", 
                tbl_id = "undergrad_id", 
                person_id = usr$person_id,
                names_df = names_df)
            
            section_iii_undergrad$data <- paste0("<br>",  as.list(loc$all_df$data), "<br>")
            
            updateSelectInput(session = session,
                              "remove_list", 
                              choices = stats::setNames(loc$all_df$undergrad_id, seq_along(loc$all_df$undergrad_id)))
            
      
            
        })
       
        
        # add ####
        
        observeEvent(input$add, {
            
            checks <- stats::setNames(item_names, items)
            check_inputs(input, checks, text = i18n()$t("Zadejte"), exclude = "other|faculty|program")
            
            all_items <- collect_items(items, input)
            
            
            new_df <- prep_new_entry(
                items, 
                all_items, 
                "undergrad", 
                usr$person_id, 
                as.integer( format(Sys.Date(), "%Y"))
            )
            
            DBI::dbAppendTable(ipcas_db, "undergrad", new_df)
            
            
            loc$all_df <- transform_table(
                ipcas_db = ipcas_db, 
                tbl = "undergrad", 
                tbl_id = "undergrad_id", 
                person_id = usr$person_id,
                names_df = names_df)
          
           
            section_iii_undergrad$data <- paste0("<br>",  as.list(loc$all_df$data), "<br>")
            
            
            updateSelectInput(session = session,
                              "remove_list", 
                              choices = stats::setNames(loc$all_df$undergrad_id, seq_along(loc$all_df$undergrad_id))
            )
            
            
            
            
        })
        
        # remove  ####
        
        observeEvent(input$remove, {
            
          
            
            pool::dbExecute(ipcas_db, 
                            "DELETE FROM undergrad WHERE undergrad_id IN (?)",
                            params = list(input$remove_list))
            
            loc$all_df <- transform_table(
                ipcas_db = ipcas_db, 
                tbl = "undergrad", 
                tbl_id = "undergrad_id", 
                person_id = usr$person_id,
                names_df = names_df)
            
            section_iii_undergrad$data <- paste0("<br>",  as.list(loc$all_df$data), "<br>")
            
            updateSelectInput(session = session,
                              "remove_list", 
                              choices = stats::setNames(loc$all_df$undergrad_id, seq_along(loc$all_df$undergrad_id))
                              
            )
            
        })
        
        # Update selection options based on choices ####
        
        observeEvent(input$undergrad_school, {
            
            
            
            choices_fac <- IPCASreporter::universities %>% 
                dplyr::filter(university == input$undergrad_school &
                                  !is.na(faculty)) %>% 
                dplyr::pull(faculty) %>% 
                unique() %>% 
                sort()
            
            
            updateSelectInput(session = session,
                              "undergrad_faculty", 
                              choices = choices_fac
                              
            )
            
        })
        
        observeEvent(input$undergrad_faculty, {
            
            choices_prog_check <- IPCASreporter::universities %>% 
                dplyr::filter(university == input$undergrad_school) %>% 
                dplyr::pull(faculty) %>%
                unique()
            
            if (length(choices_prog_check)==1) {
                
                choices_prog <- IPCASreporter::universities %>% 
                    dplyr::filter(university == input$undergrad_school &
                                      !is.na(disc_program) &
                                      stringr::str_detect(type, "bakalářský|magisterský") 
                    ) %>% 
                    dplyr::pull(disc_program) %>% 
                    unique() %>% 
                    sort()
                
            } else {
                
                choices_prog <- IPCASreporter::universities %>% 
                    dplyr::filter(university == input$undergrad_school &
                                      faculty == input$undergrad_faculty &
                                      !is.na(disc_program) &
                                      stringr::str_detect(type, "bakalářský|magisterský") 
                    ) %>% 
                    dplyr::pull(disc_program) %>% 
                    unique() %>% 
                    sort()
                
            }
            
            updateSelectInput(session = session,
                              "undergrad_program", 
                              choices = choices_prog)
            
        })
        
        
        # preview ####
        
        output$section_iii_undergrad_preview <- renderText({
            if (nrow(loc$all_df)>0) {
                paste(paste0("<br>", seq_along(section_iii_undergrad$data), "."),
                      section_iii_undergrad$data)
            } else {""}
        })
        
        # translations ####
        
        observe({
        updateRadioButtons(session, 
                           "undergrad_level", 
                           label = i18n()$t("Typ studijního programu/oboru:"),
                           choiceNames = c(i18n()$t("Bakalářský studijní program"), 
                                       i18n()$t("Magisterský studijní program")),
                           choiceValues = c(
                                "Bakalářský studijní program",
                                "Magisterský studijní program"
                                            )
                           )
       updateRadioButtons(session, 
                          "undergrad_type_prednasky", 
                          label = i18n()$t("Přednášky:"),
                          choiceNames = c(i18n()$t("ano"), 
                                      i18n()$t("ne")),
                          choiceValues = c("ano", "ne"),
                          selected = "ne",
                          inline = TRUE
                          )
       updateRadioButtons(session, 
                          "undergrad_type_seminare", 
                          label = i18n()$t("Semináře:"),
                          choiceNames = c(i18n()$t("ano"), 
                                          i18n()$t("ne")),
                          choiceValues = c("ano", "ne"),
                          selected = "ne",
                          inline = TRUE
       )
       updateRadioButtons(session, 
                          "undergrad_type_cviceni", 
                          label = i18n()$t("Cvičení:"),
                          choiceNames = c(i18n()$t("ano"), 
                                          i18n()$t("ne")),
                          choiceValues = c("ano", "ne"),
                          selected = "ne",
                          inline = TRUE
       )
       updateRadioButtons(session, 
                          "undergrad_type_vedeni", 
                          label = i18n()$t("Vedení bakalářských a diplomových prací:"),
                          choiceNames = c(i18n()$t("ano"), 
                                          i18n()$t("ne")),
                          choiceValues = c("ano", "ne"),
                          selected = "ne",
                          inline = TRUE
       )
       updateRadioButtons(session, 
                          "undergrad_type_texty", 
                          label = i18n()$t("Učební texty:"),
                          choiceNames = c(i18n()$t("ano"), 
                                          i18n()$t("ne")),
                          choiceValues = c("ano", "ne"),
                          selected = "ne",
                          inline = TRUE
       )
                          
            
        })
        
        
        return(section_iii_undergrad)
        
    })
}





