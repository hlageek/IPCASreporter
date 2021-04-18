#' postgrad UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_postgrad_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    textInput("postgrad_school", label = "Název VŠ:"),
    textInput("postgrad_faculty", label = "Název fakulty:"),
    textInput("postgrad_program", label = "Název studijního programu/studijního oboru:"),
    textInput("postgrad_course", label = "Název předmětu:"),
    checkboxGroupInput("postgrad_type", label = "", choices = c("Přednášky", "Semináře", "Cvičení",  "Vedení bakalářských a diplomových prací", "Učební texty")),
    textAreaInput("postgrad_other", label = "Jiné")
 
  )
}
    
#' postgrad Server Function
#'
#' @noRd 
mod_postgrad_server <- function(input, output, session, r){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_postgrad_ui("postgrad_ui_1")
    
## To be copied in the server
# callModule(mod_postgrad_server, "postgrad_ui_1")
 
