
#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
    # calling the translator sent as a golem option
    translator <- golem::get_golem_options(which = "translator")
    translator$set_translation_language("cz")

i18n_r <- reactive({

    selected <- input$lang
    if (length(selected) > 0 && selected %in% translator$get_languages()) {
        translator$set_translation_language(selected)
    }
    translator
})

    observeEvent(input[["lang"]], {
         shiny.i18n::update_lang(language = input[["lang"]], session = session)
         i18n_r()$set_translation_language(input[["lang"]])
    })

    usr <- reactiveValues()
   
    usr <- shinymanager::secure_server(
        check_credentials = shinymanager::check_credentials(
            db = golem::get_golem_options(which = "credentials_path"),
            passphrase = golem::get_golem_options(which = "credentials_pass")),
        timeout = 60
    )  

    identification <- mod_identification_server("identification_ui_1", usr, i18n_r)

    section_i <- mod_pub_server("pub_ui_1", identification, usr, i18n_r)
    
    section_ii <- mod_events_server("events_ui_1", identification, usr, i18n_r)
    
    section_iii_undergrad <- mod_undergrad_server("undergrad_ui_1", usr, i18n_r)
    
    section_iii_postgrad <- mod_postgrad_server("postgrad_ui_1", usr, i18n_r)
    
    section_iii_conference <- mod_conference_server( "conference_ui_1", usr, i18n_r)
    
    section_iii_lecture <- mod_lectures_server("lectures_ui_1", usr, i18n_r)
    
    section_iv <- mod_grants_server("grants_ui_1", usr, i18n_r)
    
    section_v <- mod_av21_server("av21_ui_1", usr, i18n_r)
    
    section_vi_popular <- mod_popular_server("popular_ui_1", usr, i18n_r)
    
    section_vi_school <- mod_school_server("school_ui_1", usr, i18n_r)
    
    section_vi_media <- mod_media_server("media_ui_1", usr, i18n_r)
    
    section_vii <- mod_public_server("public_ui_1", usr, i18n_r)
    
    section_viii_int_projects <-  mod_int_projects_server("int_projects_ui_1", usr, i18n_r)
    
    section_viii_int_bilateral <-  mod_int_bilateral_server("int_bilateral_ui_1", usr, i18n_r)
    
    section_ix_award <- mod_other_award_server("other_award_ui_1", usr, i18n_r)

    section_ix_review <- mod_other_review_server("other_review_ui_1", usr, i18n_r)
    
    section_ix_member <- mod_other_member_server("other_member_ui_1", usr, i18n_r)
    
    section_ix_editions <- mod_other_editions_server("other_editions_ui_1", usr, i18n_r)
    
    section_x <- mod_wip_server("wip_ui_1", usr, i18n_r)
    
    section_xi <- mod_various_server("various_ui_1", usr, i18n_r)
    

    mod_preview_server("preview_ui_1",
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
                       section_xi,
                       usr
                       )
    
    mod_manager_server("manager_1",
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
                       section_xi,
                       usr
    )
    
    mod_docx_server("docx_ui_1", 
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
                    )
    
    mod_guide_server("guide_ui_1", i18n_r)

    # deadline handler ####
    deadline <- golem::get_golem_options(which = "deadline")
    observeEvent(deadline, {
    if (isTruthy(deadline) && Sys.Date() > deadline) {
    hideTab(inputId = "sections_panel", target = "section1")
    hideTab(inputId = "sections_panel", target = "section2")
    hideTab(inputId = "sections_panel", target = "section3")
    hideTab(inputId = "sections_panel", target = "section4")
    hideTab(inputId = "sections_panel", target = "section5")
    hideTab(inputId = "sections_panel", target = "section6")
    hideTab(inputId = "sections_panel", target = "section7")
    hideTab(inputId = "sections_panel", target = "section8")
    hideTab(inputId = "sections_panel", target = "section9")
    hideTab(inputId = "sections_panel", target = "section10")
    hideTab(inputId = "sections_panel", target = "section11")
    output$deadline <- renderText({
            paste("Sběr dat ukončen / Data collection due:", deadline)
            })
    } else {
           output$deadline <- renderText({""})
    }
    })
}

