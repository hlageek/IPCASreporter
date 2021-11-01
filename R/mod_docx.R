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
                   class = "btn-warning"),
    
    mod_guide_ui("guide_ui_1")
    
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
  
# ar_rok_oddeleni_jmeno #
    filename = function() {
      
      if (isTruthy(identification$department)) {
        
        multiple_dpt <- trimws(unlist(strsplit(identification$department, split = ";")))
        
        multiple_dpt_list <- list()
        
        for (i in seq_along(multiple_dpt)) {
        multiple_dpt_list <- c(multiple_dpt_list,
                               IPCASreporter::departments %>% 
        dplyr::filter(department_name == multiple_dpt[i]) %>% 
        dplyr::pull(department_abbrev))
        }
        
        department_abbrev <- paste(unlist(multiple_dpt_list), collapse = "-")
      
      } else {
        
      department_abbrev <- "no_dpt"
        
      }
      
      if (isTruthy(identification$employee_name)) {
        
        person_name <- stringr::str_replace_all(identification$employee_name,
                               " {1,}",  
                               "_")
      } else {
        
        person_name <- "no_name"
      }
        
      paste0("ar-", 
             format(Sys.Date(), "%Y"),
             "-",
             department_abbrev,
             "-",
             person_name,
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
    
      showModal(
        modalDialog(
          title = "Notification",
          "Submissions are disabled in the test mode.", 
          easyClose = TRUE
        ))
    # req(identification$email)
    # 
    # tmp_doc <- paste0("ar-", 
    #                   format(Sys.Date(), "%Y"),
    #                   "-",
    #                   PCASreporter::departments %>% 
    #                     dplyr::filter(department_name == identification$department) %>% 
    #                     dplyr::pull(department_abbrev),
    #                   "-",
    #                   stringr::str_replace_all(identification$employee_name,
    #                                            " {1,}",  
    #                                            "_"),
    #                   ".docx")
    # 
    # print(doc(), target = tmp_doc)
    # on.exit(file.remove(tmp_doc))
    # 
    # email <- emayili::envelope() %>%
    #   emayili::from("flu.avcr@gmail.com") %>%
    #   emayili::to(identification$email) %>%
    #   emayili::cc(identification$email,
    #               departments %>% 
    #                 dplyr::filter(department_name == identification$department) %>% 
    #                 dplyr::pull(head_email)) %>% 
    #   emayili::subject("Výroční výkaz {{identification$employee_name}}") %>%
    #   emayili::text("Výroční výkaz {{identification$employee_name}} je v příloze.") %>% 
    #   emayili::attachment(tmp_doc)
    # 
    # smtp <- emayili::server(
    #   host = "smtps://smtp.gmail.com",
    #   port = 465,
    #   username = "flu.avcr@gmail.com",
    #   reuse = FALSE,
    #   password = keyring::key_get(service = "flumail",
    #                               username = "flu.avcr"),
    #   max_times = 1
    # )
    # 
    # smtp(email, verbose = FALSE)
    # 
    # 
    # showModal(
    #   modalDialog(
    #     title = "Confirmation",
    #     "Your report has been submitted.<br>You should receive a confirmation email momentarily.", 
    #     easyClose = TRUE
    #   )
    #   )
    }
  })
   
  })


}
    

 
