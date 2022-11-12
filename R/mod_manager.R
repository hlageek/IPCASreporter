#' manager UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_manager_ui <- function(id, i18n){
  ns <- NS(id)

      tabsetPanel(
          tabPanel("Osobní náhled",
                   preview_standard(ns, i18n)
                   ),
          tabPanel("Náhled oddělení")
      )
  
}
    
#' manager Server Functions
#'
#' @noRd 
mod_manager_server <- function(id,
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
                               usr){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    
    
    # Identification  ####
    output$employee_name <- renderText({identification$employee_name})
    output$department <- renderText({identification$department})
    output$fte <- renderText({identification$fte})
    output$email <- renderText({identification$email})
    output$comment <- renderText({identification$comment})
    
    
    # Section I  ####
    
    
    output$section_i <- renderText({
        paste(section_i$publist, collapse = "<br>")
    })
    
    # Section II  ####
    
    output$section_ii <- renderText({
        paste(section_ii$eventlist, collapse = "<br>")
    })
    
    
    # Section III  ####
    
    ## Undergrad  ####
    
    
    output$section_iii_undergrad <- renderText({
        paste(section_iii_undergrad$data)
    })
    
    ## Postgrad  ####
    
    output$section_iii_postgrad <- renderText({
        paste(section_iii_postgrad$data)
    })
    
    ## Conference  ####
    
    output$section_iii_conference_foreign <-  renderText({
        paste(section_iii_conference$foreign)
    })
    output$section_iii_conference_domestic <-  renderText({
        paste(section_iii_conference$domestic)
    })
    
    ## Lecture  ####
    
    output$section_iii_lecture_foreign <-  renderText({
        paste(section_iii_lecture$foreign)
    })
    output$section_iii_lecture_domestic <-  renderText({
        paste(section_iii_lecture$domestic)
    })
    
    
    # Section IV  ####
    
    output$section_iv_funded <-  renderText({
        paste(section_iv$funded)
    })
    output$section_iv_unfunded <-  renderText({
        paste(section_iv$unfunded)
    })
    
    # Section V ####
    
    output$section_v <-  renderText({
        paste(section_v$av21)
    })
    
    # Section VI  ####
    
    ## Events  ####
    
    output$section_vi_popular_events <-  renderText({
        paste(section_vi_popular$events)
    })
    
    ## School events  ####
    
    output$section_vi_school_events <-  renderText({
        paste(section_vi_school$events)
    })
    
    ## Media  ####
    
    output$section_vi_media <-  renderText({
        paste(section_vi_media$media)
    })
    
    # Section VII  ####
    
    output$section_vii <-  renderText({
        paste(section_vii$public)
    })
    
    # Section VIII  ####
    
    output$section_viii_int_projects <-  renderText({
        paste(section_viii_int_projects$projects)
    })
    
    output$section_viii_int_bilateral <-  renderText({
        paste(section_viii_int_bilateral$bilateral)
    })
    
    # Section IX  ####
    
    output$section_ix_award <-  renderText({
        paste(section_ix_award$award)
    })
    
    output$section_ix_review <-  renderText({
        paste(section_ix_review$review)
    })    
    
    output$section_ix_member_domestic <-  renderText({
        paste(section_ix_member$domestic)
    })    
    
    output$section_ix_member_foreign <-  renderText({
        paste(section_ix_member$foreign)
    })        
    
    output$section_ix_editions <-  renderText({
        paste(section_ix_editions$editions)
    })       
    
    # Section X  ####
    
    output$section_x <-  renderText({
        paste(section_x$wip)
    })
    
    # Section XI  ####
    
    output$section_xi <-  renderText({
        paste(section_xi$various)
    })
    
    
  })
}
    
## To be copied in the UI
# mod_manager_ui("manager_1")

## To be copied in the server
# mod_manager_server("manager_1")
