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
    
    textInput("undergrad_school", label = "Název VŠ:"),
    textInput("undergrad_faculty", label = "Název fakulty:"),
    textInput("undergrad_program", label = "Název studijního programu/studijního oboru:"),
    textInput("undergrad_year", label = "Akademický rok, semestr:"),
    textInput("undergrad_course", label = "Název předmětu:"),
    radioButtons("undergrad_level", label = "", choices = c("Bakalářský program:", "Magisterský program:"), inline = TRUE),
    checkboxGroupInput("undergrad_type", label = "", choices = c("Přednášky", "Semináře", "Cvičení",  "Vedení bakalářských a diplomových prací", "Učební texty")),
    numericInput("undergrad_hours", label = "Počet odučených hodin:", value = 1),
    textAreaInput("undergrad_other", label = "Jiné")
 
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
 
