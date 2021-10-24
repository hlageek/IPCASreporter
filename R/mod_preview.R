#' preview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_preview_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    
    tags$style(HTML("
                  #preview {
                    border: none;
                    height:100vh;
                    overflow-y:scroll
                  }
                  ")),    
    div(id = "preview",
    br(),
    h4("IDENTIFIKAČNÍ ÚDAJE"),
    "Jméno:",
    textOutput(ns("employee_name"), inline = TRUE),
    
    br(),
    "Oddělení:",
    textOutput(ns("department"), inline = TRUE),
    
    br(),
    "FTE:",
    textOutput(ns("fte"), inline = TRUE),
    
    br(),
    "E-mail:",
    textOutput(ns("email"), inline = TRUE),
    
    br(),
    "Komentář:",
    textOutput(ns("comment"), inline = TRUE),
    
    
    br(),
    h4("I. VYDANÉ PUBLIKACE"),
    htmlOutput(ns("section_i"), inline = FALSE),
    
    br(),
    h4("II. ORGANIZACE KONFERENCÍ A WORKSHOPŮ"),
    htmlOutput(ns("section_ii"), inline = FALSE),
    
    br(),
    h4("III. PEDAGOGICKÁ A PŘEDNÁŠKOVÁ ČINNOST"),
    h5("1) Výuka na vysokých školách a vedení prací"),
    htmlOutput(ns("section_iii_undergrad"), inline = FALSE),
    htmlOutput(ns("section_iii_postgrad"), inline = FALSE),
    
    
    br(),
    h5("2) Příspěvky a přednášky na konferencích"),
    htmlOutput(ns("section_iii_conference_foreign"), inline = FALSE),
    htmlOutput(ns("section_iii_conference_domestic"), inline = FALSE),
    
    br(),
    h5("3) Samostatné přednášky"),
    htmlOutput(ns("section_iii_lecture_foreign"), inline = FALSE),
    htmlOutput(ns("section_iii_lecture_domestic"), inline = FALSE),
    
    br(),
    h4("IV. ŘEŠENÉ ČI SPOLUŘEŠENÉ GRANTY"),
    h5("Řešené či spoluřešené granty"),
    htmlOutput(ns("section_iv_funded"), inline = FALSE),
    h5("Projekty podané a nepřijaté k financování"),
    htmlOutput(ns("section_iv_unfunded"), inline = FALSE),
    
    br(),
    h4("V. ŘEŠENÉ PROJEKTY V RÁMCI STRATEGIE AV 21"),
    htmlOutput(ns("section_v"), inline = FALSE),
    
    br(),
    h4("VI. POPULARIZAČNÍ ČINNOST"),
    h5("Akce"),
    htmlOutput(ns("section_vi_popular_events"), inline = FALSE),
    h5("Přednášky na středních, případně základních školách"),
    htmlOutput(ns("section_vi_school_events"), inline = FALSE),
    
    br(),
    h4("VII. SPOLUPRÁCE SE STÁTNÍ A VEŘEJNOU SPRÁVOU"),
    htmlOutput(ns("section_vii"), inline = FALSE),
    
    br(),
    h4("VIII. ZAHRANIČNÍ SPOLUPRÁCE"),
    h5("Zapojení do mezinárodních projektů"),
    htmlOutput(ns("section_viii_int_projects"), inline = FALSE),
    h5("Mezinárodní dvoustranné dohody"),
    htmlOutput(ns("section_viii_int_bilateral"), inline = FALSE),
    
    br(),
    h4("IX. OSTATNÍ"),
    h5("Ocenění odbornou komunitou"),
    htmlOutput(ns("section_ix_award"), inline = FALSE),
    h5("Posudky"),
    htmlOutput(ns("section_ix_review"), inline = FALSE),
    h5("Členství v komisích, redakčních radách apod."),
    h6("Domácí"),
    htmlOutput(ns("section_ix_member_domestic"), inline = FALSE),
    h6("Zahraniční"),
    htmlOutput(ns("section_ix_member_foreign"), inline = FALSE),
    
    br(),
    h4("X. ROZPRACOVANÉ PUBLIKACE A PROJEKTY"),
    htmlOutput(ns("section_x"), inline = FALSE),
    
    br(),
    h4("XI. RŮZNÉ"),
    htmlOutput(ns("section_xi"), inline = FALSE)
)
  )
}
    
#' preview Server Function
#'
#' @noRd 
mod_preview_server <- function(id,
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
                               ) {
 
  moduleServer(id, function(input, output, session) {
    
    # Identification
    output$employee_name <- renderText({identification$employee_name})
    output$department <- renderText({identification$department})
    output$fte <- renderText({identification$fte})
    output$email <- renderText({identification$email})
    output$comment <- renderText({identification$comment})
    
    
    # Section I
    
    
    output$section_i <- renderText({
      paste(section_i$publist, collapse = "<br>")
      })
    
    # Section II
    
    output$section_ii <- renderText({
      paste(section_ii$eventlist, collapse = "<br>")
    })
    
    
    # Section III
    
    ## Undergrad
    
    
        output$section_iii_undergrad <- renderText({
          paste(section_iii_undergrad$data)
      })
        
    ## Postgrad
        
        output$section_iii_postgrad <- renderText({
          paste(section_iii_postgrad$data)
        })
        
    ## Conference
        
        output$section_iii_conference_foreign <-  renderText({
          paste(section_iii_conference$foreign)
        })
        output$section_iii_conference_domestic <-  renderText({
          paste(section_iii_conference$domestic)
        })
        
    ## Lecture
        
        output$section_iii_lecture_foreign <-  renderText({
          paste(section_iii_lecture$foreign)
        })
        output$section_iii_lecture_domestic <-  renderText({
          paste(section_iii_lecture$domestic)
        })
        
        
    # Section IV
        
        output$section_iv_funded <-  renderText({
          paste(section_iv$funded)
        })
        output$section_iv_unfunded <-  renderText({
          paste(section_iv$unfunded)
        })
        
    # Section V
        
        output$section_v <-  renderText({
          paste(section_v$av21)
        })
        
    # Section VI
        
        ## Events
        
        output$section_vi_popular_events <-  renderText({
          paste(section_vi_popular$events)
        })
        
        ## School events
        
        output$section_vi_school_events <-  renderText({
          paste(section_vi_school$events)
        })
        
    # Section VII
        
        output$section_vii <-  renderText({
          paste(section_vii$public)
        })
    
    # Section VIII
        
        output$section_viii_int_projects <-  renderText({
          paste(section_viii_int_projects$projects)
        })
        
        output$section_viii_int_bilateral <-  renderText({
          paste(section_viii_int_bilateral$bilateral)
        })
        
    # Section IX
        
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
        
        # Section X
        
        output$section_x <-  renderText({
          paste(section_x$wip)
        })
        
        # Section XI
        
        output$section_xi <-  renderText({
          paste(section_xi$data)
        })
  }
  
  )
}

    
