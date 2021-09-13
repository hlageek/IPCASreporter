identificationApp <- function() {
    ui <- fluidPage(
        mod_identification_ui("identification_ui_1")
    )
    server <- function(input, output, session) {
        
        mod_identification_server("identification_ui_1")
    }
    shinyApp(ui, server)  
}
