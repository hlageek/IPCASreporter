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
mod_docx_server <- function(input, output, session, r){
  ns <- session$ns
  
  output$download_docx<- downloadHandler(
  
    filename = function() {
      tempfile(fileext = ".docx")
    },
    
    content = function(file) {
      doc <- officer::read_docx(here::here("inst", "app", "www", "annual_report_ipcas.docx")) %>% 
        officer::body_replace_text_at_bkm("employee_name", r$employee_name) %>% 
        officer::body_replace_text_at_bkm("department",r$department) %>% 
        officer::body_replace_text_at_bkm("fte", r$fte) %>% 
        officer::body_replace_text_at_bkm("pubs", r$pubs) 
        

      print(doc, target = file)
    }
   
  )
 
}
    
## To be copied in the UI
# mod_docx_ui("docx_ui_1")
    
## To be copied in the server
# callModule(mod_docx_server, "docx_ui_1")
 
