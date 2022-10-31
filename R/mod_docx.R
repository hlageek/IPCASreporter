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
          "Are you sure you want to continue?",
          title = "Submission",
          footer = tagList(
            actionButton(ns("cancel"), "Cancel"),
            actionButton(ns("ok"), "Submit", class = "btn-success")
        ))
        )
    }
    })
  
  observeEvent(input$cancel, {
    removeModal()
  })
  
  observeEvent(input$ok, {
    
      email_password <- golem::get_golem_options(which = "email_password")
      golem::cat_dev(email_password)
      
      if (!is.null(email_password)) {
          
    tmp_doc <- paste0("ar-",
                      format(Sys.Date(), "%Y"),
                      "-",
                      IPCASreporter::departments %>%
                        dplyr::filter(department_name == identification$department) %>%
                        dplyr::pull(department_abbrev),
                      "-",
                      stringr::str_replace_all(identification$employee_name,
                                               " {1,}",
                                               "_"),
                      ".docx")
    
    print(doc(), target = tmp_doc)
    on.exit(file.remove(tmp_doc))
    mailR::send.mail(from = "no-reply@flu.cas.cz",
                     to = identification$email,
                     cc = golem::get_golem_options(which = "email_default"),
                     subject = paste("Výroční výkaz", identification$employee_name),
                     body = paste("Výroční výkaz", identification$employee_name, "je v příloze."),
                     encoding = "utf-8",
                     smtp = list(
                         host.name = "mail.flu.cas.cz", 
                         port = 25, 
                         user.name = "hladik@flu.cas.cz", 
                         passwd = email_password, 
                         tls = TRUE),
                     authenticate = FALSE,
                     send = TRUE,
                     attach.files = tmp_doc)
    
    
    showModal(
      modalDialog(
        title = "Confirmation",
        "Your report has been submitted. You should receive a confirmation email momentarily.",
        easyClose = TRUE
      )
    )
    
    showNotification("Report submitted.")
    removeModal()
      }
  })

    
  })
  }
   
 
