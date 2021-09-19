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
  tagList(
    
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
    
    radioButtons(ns("undergrad_level"), label = "", choices = c("Bakalářský studijní program", "Magisterský studijní program"), inline = TRUE),
    
    textInput(ns("undergrad_course"), label = "Název předmětu:"),

    checkboxGroupInput(ns("undergrad_type"), label = "", choices = c("Přednášky", "Semináře", "Cvičení",  "Vedení bakalářských a diplomových prací", "Učební texty")),
    numericInput(ns("undergrad_hours"), label = "Počet odučených hodin:", value = 1),
    textAreaInput(ns("undergrad_other"), label = "Jiné"),
    mod_add_remove_ui(ns("add_remove_ui_1"))
    
 
  )
}
    
#' undergrad Server Function
#'
#' @noRd 
mod_undergrad_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    

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
 
  })
  }
    
## To be copied in the UI
# mod_undergrad_ui("undergrad_ui_1")
    
## To be copied in the server
# callModule(mod_undergrad_server, "undergrad_ui_1")
 
