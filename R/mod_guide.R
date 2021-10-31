#' guide UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_guide_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    actionButton(ns("show_guide"),
      label = "Instructions",
      icon = icon("info-circle"))
 
  )
}
    
#' guide Server Function
#'
#' @noRd 
mod_guide_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$show_guide, {
    showModal(modalDialog(
      size = "l",
      footer = tagList(
        modalButton("Close")),
      easyClose = TRUE,
      
      tagList(
        
        h2("Instructions"),
        
        tags$li("Your name will be used to search the ASEP repository to provide selectable options in sections", tags$b( "I. & II.")),
        
        tags$li("Use the menu on the left to navigate in the application and continue to fill all relevant", tags$b("report sections.")),
        
        tags$li("Use the", tags$b("Save"), "button to generate a link that will restore your work on the report if you need to come back to it later."),
        
        tags$li("Use the", tags$b("Download"), "button to generate a MS Word version of the report."),
        
        tags$li("Use the", tags$b("Submit"), "button to submit the report. You will receive a confirmation email after the submission."),
        
        tags$li(HTML("<i class='fa fa-warning'></i>"), "To avoid data loss, do not refresh the browser while using the app!", style = "color:red"),
        
        h3("Navigation"),
        
        img(src="https://owncloud.cesnet.cz/index.php/s/sEf5XIUg7d0jNKq/download",
            width="100%"),
        
        tags$ol(
          tags$li(tags$b("Vertical navigation"), "for the application and switching between", tags$b("Preview"), "and", tags$b("Report sections"), "."),
          
          tags$li(tags$b("Horizontal navigation"), "for the inside of the report sections. (Only appears in subdivided sections.)"),
          
          tags$li(tags$b("Section input"), "panel for adding individual report items."),
          
          tags$li(tags$b("Section preview"), "to display previous inputs. Selected inputs can be deleted here.")
        ),
        
        h2("Help"),
        
        p("If you need help or have a comment, please leave a message in the ", tags$b("#technicka_podpora"), "channel at", a("https://filosoficky.slack.com", href = "https://filosoficky.slack.com/"), ".")
        
        
        
          
        )
        
))
    
    })
  }
  
  )}
    

