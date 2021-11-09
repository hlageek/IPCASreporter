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
                choices = c("", unique(sort(universities$university))), 
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
                      label = "Remove item from report"
         )
         
         
  )
  )
}
    
#' undergrad Server Function
#'
#' @noRd 
mod_undergrad_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    section_iii_undergrad <- reactiveValues()
    
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
      
    
    
    observeEvent(input$add, {
      
      all_items <- list()
      
      for (i in seq_along(items)) {
        
        all_items <- c(all_items, paste(item_names[i], input[[items[i]]]))
        
      }
      
      

      section_iii_undergrad$data[[length(section_iii_undergrad$data)+1]] <- paste(c(all_items,"<br>"), collapse = "<br>")
      
      updateSelectInput(session = session,
                        "remove_list", 
                        choices = seq_along(section_iii_undergrad$data)
                        )
                        
      
      

    })
    
    observeEvent(input$remove, {
      
      
      section_iii_undergrad$data[as.integer(input$remove_list)] <- NULL 
      
      updateSelectInput(session = session,
                        "remove_list", 
                        choices = seq_along(section_iii_undergrad$data)
                        
      )
      
    })
    
    # Update selection options based on choices

    observe({
      
      if (isTruthy(input$undergrad_school)) {
        
        choices_fac <- universities %>% 
          dplyr::filter(university == input$undergrad_school &
                          !is.na(faculty)) %>% 
          dplyr::pull(faculty) %>% 
          unique() %>% 
          sort()
        
        
  updateSelectInput(session = session,
                    "undergrad_faculty", 
                    choices = choices_fac
                      
                    )
      }
    })
      
      observe({
      
        choices_prog_check <- universities %>% 
          dplyr::filter(university == input$undergrad_school) %>% 
          dplyr::pull(faculty) %>%
          unique()
        
        if (length(choices_prog_check)==1) {
          
          choices_prog <- universities %>% 
            dplyr::filter(university == input$undergrad_school &
                            !is.na(disc_program) &
                            stringr::str_detect(type, "bakalářský|magisterský") 
                          ) %>% 
            dplyr::pull(disc_program) %>% 
            unique() %>% 
            sort()
          
        } else {
          
          choices_prog <- universities %>% 
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
      
      
     
        
        output$section_iii_undergrad_preview <- renderText({
          if (length(section_iii_undergrad$data)>0) {
          paste(paste0(seq_along(section_iii_undergrad$data), ".<br>"),
                section_iii_undergrad$data)
          } else {""}
          })
      
      
        
      

      
      
      return(section_iii_undergrad)
 
  })
}
    



 
