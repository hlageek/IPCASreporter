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
        
      
        br(),
        tags$li("After you have filled your name in the", tags$b( "researcher's details"), "section a preview of your report will appear here."),
        
        tags$li("Your name will be used to search the ASEP repository to provide selectable options in sections", tags$b( "I. & II.")),
        
        tags$li("Use the menu on the left to navigate in the application and continue to fill all relevant", tags$b("report sections.")),
        
        tags$li("Use the", tags$b("Save"), "button to generate a link that will restore your work on the report if you need to come back to it later."),
        
        tags$li("Use the", tags$b("Download"), "button to generate a MS Word version of the report."),
        
        tags$li("Use the", tags$b("Submit"), "button to submit the report. You will receive a confirmation email after the submission."),
        
        tags$li(HTML("<i class='fa fa-warning'></i>"), "To avoid data loss, do not refresh the browser while using the app!", style = "color:red"),
        
        
        p("Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Proin in tellus sit amet nibh dignissim sagittis. Etiam posuere lacus quis dolor. Phasellus et lorem id felis nonummy placerat. Phasellus rhoncus. In convallis. Aenean id metus id velit ullamcorper pulvinar. Proin in tellus sit amet nibh dignissim sagittis. Vivamus luctus egestas leo. Fusce dui leo, imperdiet in, aliquam sit amet, feugiat eu, orci. Aenean placerat. Curabitur ligula sapien, pulvinar a vestibulum quis, facilisis vel sapien. Morbi leo mi, nonummy eget tristique non, rhoncus non leo."),
        
        h3("Test"),
        
        p("  Etiam posuere lacus quis dolor. Nullam justo enim, consectetuer nec, ullamcorper ac, vestibulum in, elit. Sed ac dolor sit amet purus malesuada congue. Aliquam erat volutpat. Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur? Curabitur bibendum justo non orci. Pellentesque sapien. Aliquam erat volutpat. Etiam posuere lacus quis dolor. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Aliquam erat volutpat. Maecenas aliquet accumsan leo. Morbi leo mi, nonummy eget tristique non, rhoncus non leo. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Morbi scelerisque luctus velit. Praesent id justo in neque elementum ultrices."),
        
        p("  Etiam posuere lacus quis dolor. Nullam justo enim, consectetuer nec, ullamcorper ac, vestibulum in, elit. Sed ac dolor sit amet purus malesuada congue. Aliquam erat volutpat. Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur? Curabitur bibendum justo non orci. Pellentesque sapien. Aliquam erat volutpat. Etiam posuere lacus quis dolor. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Aliquam erat volutpat. Maecenas aliquet accumsan leo. Morbi leo mi, nonummy eget tristique non, rhoncus non leo. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Morbi scelerisque luctus velit. Praesent id justo in neque elementum ultrices."),
        
        p("  Etiam posuere lacus quis dolor. Nullam justo enim, consectetuer nec, ullamcorper ac, vestibulum in, elit. Sed ac dolor sit amet purus malesuada congue. Aliquam erat volutpat. Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur? Curabitur bibendum justo non orci. Pellentesque sapien. Aliquam erat volutpat. Etiam posuere lacus quis dolor. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Aliquam erat volutpat. Maecenas aliquet accumsan leo. Morbi leo mi, nonummy eget tristique non, rhoncus non leo. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Morbi scelerisque luctus velit. Praesent id justo in neque elementum ultrices.")
        
        
          
        )
        
))
    
    })
  }
  
  )}
    

