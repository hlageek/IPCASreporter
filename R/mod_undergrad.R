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
    
    textInput(ns("undergrad_school"), label = "Název VŠ:"),
    textInput(ns("undergrad_faculty"), label = "Název fakulty:"),
    textInput(ns("undergrad_program"), label = "Název studijního programu/studijního oboru:"),
    textInput(ns("undergrad_year"), label = "Akademický rok, semestr:"),
    textInput(ns("undergrad_course"), label = "Název předmětu:"),
    radioButtons(ns("undergrad_level"), label = "", choices = c("Bakalářský studijní program", "Magisterský studijní program"), inline = TRUE),
    checkboxGroupInput(ns("undergrad_type"), label = "", choices = c("Přednášky", "Semináře", "Cvičení",  "Vedení bakalářských a diplomových prací", "Učební texty")),
    numericInput(ns("undergrad_hours"), label = "Počet odučených hodin:", value = 1),
    textAreaInput(ns("undergrad_other"), label = "Jiné"),
    mod_add_remove_ui(ns("add_remove_ui_1"))
    
 
  )
}
    
#' undergrad Server Function
#'
#' @noRd 
mod_undergrad_server <- function(input, output, session, r){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_undergrad_ui("undergrad_ui_1")
    
## To be copied in the server
# callModule(mod_undergrad_server, "undergrad_ui_1")
 
