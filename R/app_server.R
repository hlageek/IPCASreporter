#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
    

    identification <- mod_identification_server("identification_ui_1")

    section_i <- mod_pub_server("pub_ui_1", identification)
    
    section_ii <- mod_events_server("events_ui_1", identification)
    
    section_iii_undergrad <- mod_undergrad_server("undergrad_ui_1")
    
    section_iii_postgrad <- mod_postgrad_server("postgrad_ui_1")
    
    section_iii_conference <- mod_conference_server( "conference_ui_1")
    
    section_iii_lecture <- mod_lectures_server("lectures_ui_1")
    
    section_iv <- mod_grants_server("grants_ui_1")
    
    section_v <- mod_av21_server("av21_ui_1")
    
    section_vi_popular <- mod_popular_server("popular_ui_1")
    
    section_vi_school <- mod_school_server("school_ui_1")
    
    section_vi_media <- mod_media_server("media_ui_1")
    
    section_vii <- mod_public_server("public_ui_1")
    
    section_viii_int_projects <-  mod_int_projects_server("int_projects_ui_1")
    
    section_viii_int_bilateral <-  mod_int_bilateral_server("int_bilateral_ui_1")
    section_ix_award <- mod_other_award_server("other_award_ui_1")

    section_ix_review <- mod_other_review_server("other_review_ui_1")
    
    

    
    section_ix_member <- mod_other_member_server("other_member_ui_1")
    
    section_ix_editions <- mod_other_editions_server("other_editions_ui_1")
    
    section_x <- mod_wip_server("wip_ui_1")
    
    section_xi <- mod_various_server("various_ui_1")
    
    
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
                       section_xi
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
    
    mod_guide_server("guide_ui_1")
    
}
