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
    shinyjs::useShinyjs(),
    shinyjs::disabled(
    downloadButton(ns("download_docx"), "Download")
    )
  )
}
    
#' docx Server Function
#'
#' @noRd 
mod_docx_server <- function(id, 
                            identification,
                            section_i,
                            section_ii,
                            section_iii_undergrad,
                            section_iii_postgrad,
                            section_iii_conference,
                            section_iii_lecture,
                            section_iv,
                            section_v,
                            section_vi_popular,
                            section_vi_school,
                            section_vii,
                            section_viii_int_projects,
                            section_viii_int_bilateral,
                            section_ix_award,
                            section_ix_review,
                            section_ix_member,
                            section_x,
                            section_xi
                            ){
  
  moduleServer(id, function(input, output, session) {
  ns <- session$ns
  

  doc <-  reactive({compile_docx(identification,
                                 section_i,
                                 section_ii,
                                 section_iii_undergrad,
                                 section_iii_postgrad,
                                 section_iii_conference,
                                 section_iii_lecture,
                                 section_iv,
                                 section_v,
                                 section_vi_popular,
                                 section_vi_school,
                                 section_vii,
                                 section_viii_int_projects,
                                 section_viii_int_bilateral,
                                 section_ix_award,
                                 section_ix_review,
                                 section_ix_member,
                                 section_x,
                                 section_xi
                                 )})
  
  output$download_docx<- downloadHandler(
  

    filename = function() {
      paste0("ipcas_annual_report-", identification$employee_name, ".docx")
    },
    
    content = function(file) {
      doc <- doc()
        
      
      print(doc, target = file)
    }
   
  )

  observeEvent(identification$employee_name, {
    if (identification$employee_name == "")
      shinyjs::disable("download_docx")
    else
      shinyjs::enable("download_docx")
  })
   
   
  })
  
  
}
    

 
