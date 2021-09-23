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
  

  
  fluidRow(column(width = 4,
    
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
                 label = "Update report"
    )
 
  ),
  
  column(width = 8,
         
         
         h3("1)	Výuka na vysokých školách a vedení prací:"),
         h4("a) Bakalářské a magisterské studijní programy "),
         
         htmlOutput(ns("section_iii_undergrad"), inline = FALSE),
         
         
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
      "undergrad_type",
      "undergrad_hours",
      "undergrad_other"
      )
      
      
    
    observeEvent(input$add, {
      
      all_items <- list()
      
      for (i in seq_along(items)) {
        
        all_items <- c(all_items, input[[items[i]]])
        
      }
      
      section_iii_undergrad[[as.character(input$add)]] <- paste(all_items, collapse = "<br>")
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
      
      
      if (isTruthy(section_iii_undergrad)) {
        
        output$section_iii_undergrad <- renderText({
          paste(reactiveValuesToList(section_iii_undergrad))
        })
        
      }

      
      
      return(section_iii_undergrad)
 
  })
}
    



 
