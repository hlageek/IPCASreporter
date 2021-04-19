#' docx UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_docx_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    downloadButton(NS(id, "download_docx"), "Download")
    
  )
}
    
#' docx Server Function
#'
#' @noRd 
mod_docx_server <- function(id, 
                            r,
                            employee_name,
                            department){
  moduleServer(id, function(input, output, session) {
  ns <- session$ns
  
 
  doc <-  reactive({compile_docx(r,
                                 employee_name,
                                 department)})
  
  output$download_docx<- downloadHandler(
  
    filename = function() {
      paste0("ipcas_annual_report-", employee_name, ".docx")
    },
    
    content = function(file) {
      doc <- doc()
        

      print(doc, target = file)
    }
   
  )
 
  })
}
    
## To be copied in the UI
# mod_docx_ui("docx_ui_1")
    
## To be copied in the server
# callModule(mod_docx_server, "docx_ui_1")
 
