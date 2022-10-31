#' undergrad UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_undergrad_ui <- function(id){
    ns <- NS(id)
    
    
    
    fluidRow(column(width = 6,
                    
                    selectInput(ns("undergrad_school"), 
                                label = "Název VŠ:", 
                                choices = NULL, 
                                selected = ""),
                    
                    selectInput(ns("undergrad_faculty"), 
                                label = "Název fakulty:",
                                choices = NULL,
                                selected = ""),
                    selectInput(ns("undergrad_program"), 
                                label = "Název studijního programu/studijního oboru:",
                                choices = NULL,
                                selected = ""),
                    
                    selectInput(ns("undergrad_year"), 
                                label = "Akademický rok, semestr:",
                                choices = c(paste0(format(Sys.Date(), "%Y"), ", LS"),
                                            paste0(format(Sys.Date(), "%Y"), ", ZS"))),
                    
                    radioButtons(ns("undergrad_level"), 
                                 label = "", 
                                 choices = c("Bakalářský studijní program", 
                                             "Magisterský studijní program"), 
                                 inline = TRUE),
                    
                    textInput(ns("undergrad_course"), 
                              label = "Název předmětu:"),
                    
                    
                    undergrad_types(id), tags$br(),
                    
                    
                    numericInput(ns("undergrad_hours"), 
                                 label = "Počet odučených hodin:", 
                                 value = 0,
                                 min = 0,
                                 step = 1),
                    
                    textAreaInput(ns("undergrad_other"), 
                                  label = "Jiné"),
                    
                    actionButton(ns("add"),
                                 label = "Add to report",                  icon = icon("check"),                  class = "btn-success"
                    )
                    
    ),
    
    column(width = 6,
           
           
           h3("1)	Výuka na vysokých školách a vedení prací:"),
           h4("a) Bakalářské a magisterské studijní programy "),
           
           htmlOutput(ns("section_iii_undergrad_preview"), inline = FALSE),
           
           selectInput(ns("remove_list"), 
                       label = "Item",
                       choices = ""),
           actionButton(ns("remove"),
                        label = "Remove from report", class = "btn-primary", icon = icon("trash")
           )
           
           
    )
    )
}

#' undergrad Server Function
#'
#' @noRd 
mod_undergrad_server <- function(id, usr) {
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
        
        #  on startup ####
        
        observeEvent(usr$person_id, {
            
            uni_choices <- IPCASreporter::universities %>% 
                dplyr::pull(university)
            uni_choices <- unique(uni_choices)
        
            updateSelectInput(session = session,
                              "undergrad_school", 
                              choices =  c("", sort(uni_choices))
                              )
            
            loc$names <- tibble::tibble(key = items,
                                        names = item_names)
            
            loc$all_df <-  ipcas_db %>% 
                dplyr::tbl("undergrad") %>% 
                dplyr::filter(person_id_undergrad == !!usr$person_id) %>%
                dplyr::select(-person_id_undergrad) %>% 
                tidyr::pivot_longer(-undergrad_id,
                                    names_to = "key",
                                    values_to = "value") %>%
                dplyr::collect() %>% 
                dplyr::left_join(loc$names, by = "key") %>% 
                tidyr::unite("value", c(names, value), sep = " ") %>% 
                dplyr::select(-key) %>% 
                dplyr::group_by(undergrad_id) %>% 
                dplyr::summarise(data = stringr::str_flatten(value,                                                    collapse = "<br>")) 
            section_iii_undergrad$data <- paste0("<br>",  as.list(loc$all_df$data), "<br>")
            
            updateSelectInput(session = session,
                              "remove_list", 
                              choices = stats::setNames(loc$all_df$undergrad_id, seq_along(loc$all_df$undergrad_id)))
            
      
            
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
                dplyr::mutate(person_id_undergrad = usr$person_id) 
            
            DBI::dbAppendTable(ipcas_db, "undergrad", new_df)
            
            
            loc$all_df <- ipcas_db %>% 
                dplyr::tbl("undergrad") %>% 
                dplyr::filter(person_id_undergrad == !!usr$person_id) %>%
                dplyr::select(-person_id_undergrad) %>% 
                tidyr::pivot_longer(-undergrad_id,
                                    names_to = "key",
                                    values_to = "value") %>%
                dplyr::collect() %>% 
                dplyr::left_join(loc$names, by = "key") %>% 
                tidyr::unite("value", c(names, value), sep = " ") %>% 
                dplyr::select(-key) %>% 
                dplyr::group_by(undergrad_id) %>% 
                dplyr::summarise(data = stringr::str_flatten(value,                                                    collapse = "<br>")) 
                
            
          
           
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
            
            loc$all_df <-  ipcas_db %>% 
                dplyr::tbl("undergrad") %>% 
                dplyr::filter(person_id_undergrad == !!usr$person_id) %>%
                dplyr::select(-person_id_undergrad) %>% 
                tidyr::pivot_longer(-undergrad_id,
                                    names_to = "key",
                                    values_to = "value") %>%
                dplyr::collect() %>% 
                dplyr::left_join(loc$names, by = "key") %>% 
                tidyr::unite("value", c(names, value), sep = " ") %>% 
                dplyr::select(-key) %>% 
                dplyr::group_by(undergrad_id) %>% 
                dplyr::summarise(data = stringr::str_flatten(value,                                                    collapse = "<br>")) 
            
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
            if (length(section_iii_undergrad$data)>0) {
                paste(paste0("<br>", seq_along(section_iii_undergrad$data), "."),
                      section_iii_undergrad$data)
            } else {""}
        })
        
        
        return(section_iii_undergrad)
        
    })
}





