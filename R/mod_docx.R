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

    downloadButton(ns("download_docx"), "Download"),
    
    actionButton(ns("submit_docx"), 
                   icon = icon("paper-plane"), 
                   "Submit",
                   class = "btn-warning")
    
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
                            section_vi_media,
                            section_vii,
                            section_viii_int_projects,
                            section_viii_int_bilateral,
                            section_ix_award,
                            section_ix_review,
                            section_ix_member,
                            section_ix_editions,
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
                                 section_vi_media,
                                 section_vii,
                                 section_viii_int_projects,
                                 section_viii_int_bilateral,
                                 section_ix_award,
                                 section_ix_review,
                                 section_ix_member,
                                 section_ix_editions,
                                 section_x,
                                 section_xi
                                 )})
  
  output$download_docx<- downloadHandler(
  

    filename = function() {
      paste0("ipcas_annual_report-", 
             stringr::str_replace_all(identification$employee_name,
                                      " {1,}",  
                                      "_"),
             ".docx")
    },
    
    content = function(file) {
      doc <- doc()
        
      
      print(doc, target = file)
    }
   
  )


  observeEvent(input$submit_docx, {
    
    #browser()
    if (!isTruthy(identification$email) || !stringr::str_detect(identification$email, "@") ) {
      validate(
        showModal(
          modalDialog(
            title = "Warning",
            "Your email address is required.", 
            easyClose = TRUE
          )
        )
      )
    } else {
    
    req(identification$email)

    tmp_doc <- paste0("ipcas_annual_report-", 
                      stringr::str_replace_all(identification$employee_name,
                                               " {1,}",  
                                               "_"),
                      ".docx")

    print(doc(), target = tmp_doc)
    
    
    email <- emayili::envelope() %>%
      emayili::from("noreply@flu.cas.cz") %>%
      emayili::to(identification$email) %>%
      emayili::cc(identification$email) %>% 
      emayili::subject("Výroční výkaz {{identification$employee_name}}") %>%
      emayili::text("Výroční výkaz {{identification$employee_name}} je v příloze.") %>% 
      emayili::attachment(tmp_doc)
    
    smtp <- emayili::server(
      host = "smtps://smtp.gmail.com",
      port = 465,
      username = "flu.avcr@gmail.com",
      reuse = FALSE,
      password = keyring::key_get(service = "flumail",
                                  username = "flu.avcr"),
      max_times = 1
    )
    
    smtp(email, verbose = FALSE)
    
    file.remove(tmp_doc)
    
    showModal(
      modalDialog(
        title = "Confirmation",
        "Your report has been submitted.", 
        easyClose = TRUE
      )
      )
    }
  })
   
  })


}
    

 
