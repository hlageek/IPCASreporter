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

      tabsetPanel(id = ns("tabs"),
          tabPanel("Osobní náhled", value = "personal_view",
                   preview_standard(ns, i18n)
                   ),
          tabPanel("Náhled oddělení", value = "department_view",
                   
                   tags$div(style=" display: flex; align-items: center; justify-content: left;",
                   
                   selectInput(ns("department"),
                               label = i18n$t("Oddělení"),
                               selected = "",
                               choices = c("", IPCASreporter::departments$department_name),
                               multiple = FALSE
                   ),
          
                   selectInput(ns("persons"),
                               label = i18n$t("Osoby"),
                               selected = "",
                               choices = "",
                               multiple = TRUE
                   ),
                   
     
                   actionButton(ns("show_button"), "Zobrazit vybrané")
                   
                   ),
                   
                   h4(i18n$t("I. VYDANÉ PUBLIKACE")),
                   htmlOutput(ns("manager_section_i"), inline = FALSE),
                   
                   br(),
                   h4(i18n$t("II. ORGANIZACE KONFERENCÍ A WORKSHOPŮ")),
                   htmlOutput(ns("manager_section_ii"), inline = FALSE),
                   
                   br(),
                   h4(i18n$t("III. PEDAGOGICKÁ A PŘEDNÁŠKOVÁ ČINNOST")),
                   h5(i18n$t("1) Výuka na vysokých školách a vedení prací")),
                   htmlOutput(ns("manager_section_iii_undergrad"), inline = FALSE),
                   htmlOutput(ns("manager_section_iii_postgrad"), inline = FALSE),
                   
                   
                   br(),
                   h5(i18n$t("2) Příspěvky a přednášky na konferencích")),
                   h5(i18n$t("Zahraniční")),
                   htmlOutput(ns("manager_section_iii_conference_foreign"), inline = FALSE),
                   h5(i18n$t("Domácí")),
                   htmlOutput(ns("manager_section_iii_conference_domestic"), inline = FALSE),
                   
                   br(),
                   h5(i18n$t("3) Samostatné přednášky")),
                   htmlOutput(ns("manager_section_iii_lecture_foreign"), inline = FALSE),
                   htmlOutput(ns("manager_section_iii_lecture_domestic"), inline = FALSE),
                   
                   br(),
                   h4(i18n$t("IV. ŘEŠENÉ ČI SPOLUŘEŠENÉ GRANTY")),
                   h5(i18n$t("Řešené či spoluřešené granty")),
                   htmlOutput(ns("manager_section_iv_funded"), inline = FALSE),
                   h5(i18n$t("Projekty podané a nepřijaté k financování")),
                   htmlOutput(ns("manager_section_iv_unfunded"), inline = FALSE),
                   
                   br(),
                   h4(i18n$t("V. ŘEŠENÉ PROJEKTY V RÁMCI STRATEGIE AV 21")),
                   htmlOutput(ns("manager_section_v"), inline = FALSE),
                   
                   br(),
                   h4(i18n$t("VI. POPULARIZAČNÍ ČINNOST")),
                   h5(i18n$t("Akce")),
                   htmlOutput(ns("manager_section_vi_popular_events"), inline = FALSE),
                   h5(i18n$t("Přednášky na středních, případně základních školách")),
                   htmlOutput(ns("manager_section_vi_school_events"), inline = FALSE),
                   h5(i18n$t("Vystoupení a popularizační texty v médiích")),
                   htmlOutput(ns("manager_section_vi_media"), inline = FALSE),
                   
                   
                   br(),
                   h4(i18n$t("VII. SPOLUPRÁCE SE STÁTNÍ A VEŘEJNOU SPRÁVOU")),
                   htmlOutput(ns("manager_section_vii"), inline = FALSE),
                   
                   br(),
                   h4(i18n$t("VIII. ZAHRANIČNÍ SPOLUPRÁCE")),
                   h5(i18n$t("Zapojení do mezinárodních projektů")),
                   htmlOutput(ns("manager_section_viii_int_projects"), inline = FALSE),
                   h5(i18n$t("Mezinárodní dvoustranné dohody")),
                   htmlOutput(ns("manager_section_viii_int_bilateral"), inline = FALSE),
                   
                   br(),
                   h4(i18n$t("IX. OSTATNÍ")),
                   h5(i18n$t("Ocenění odbornou komunitou")),
                   htmlOutput(ns("manager_section_ix_award"), inline = FALSE),
                   h5(i18n$t("Posudky")),
                   htmlOutput(ns("manager_section_ix_review"), inline = FALSE),
                   h5(i18n$t("Odborná grémia, redakční a oborové rady apod.")),
                   h6(i18n$t("Domácí")),
                   htmlOutput(ns("manager_section_ix_member_domestic"), inline = FALSE),
                   h6(i18n$t("Zahraniční")),
                   htmlOutput(ns("manager_section_ix_member_foreign"), inline = FALSE),
                   h5(i18n$t("Redakční práce")),
                   htmlOutput(ns("manager_section_ix_editions"), inline = FALSE),
                   
                   br(),
                   h4(i18n$t("X. ROZPRACOVANÉ PUBLIKACE A PROJEKTY")),
                   htmlOutput(ns("manager_section_x"), inline = FALSE),
                   
                   br(),
                   h4(i18n$t("XI. RŮZNÉ")),
                   htmlOutput(ns("manager_section_xi"), inline = FALSE)
                   )
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
 
    loc <- reactiveValues()
    
    render_preview(output,
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
                   section_xi)
    
    observeEvent(req(input$department), {
        
        loc$dpt_people <- dplyr::tbl(ipcas_db, "departments") %>% 
            dplyr::filter(department == !!input$department) %>% 
            dplyr::pull(person_id_departments)
        
        loc$people <- dplyr::tbl(ipcas_db, "persons") %>% 
            dplyr::filter(person_id %in% !!loc$dpt_people) %>% 
            dplyr::arrange(name_last) %>% 
            dplyr::collect()
        
        updateSelectInput(session = session,
                          "persons",
                          selected = loc$people$person_id,
                          choices = stats::setNames(
                              loc$people$person_id,
                              paste(loc$people$name_first, loc$people$name_last))
                          )
    })
    
    observeEvent({
        input$show_button
        req(input$department)
    }, {
        # browser()
        
        # section I ####
        
        manager_section_i <- 
            present_table(
                ipcas_db = ipcas_db,
                person_id = loc$dpt_people,
                tbl = "pubs",
                tbl_id = "pub_id",
                filter_col = NULL,
                filter_val = NULL,
                names_df = names_df_switch("pubs"),
                person_id_selected = input$persons,
                dpt_people = loc$people
            )
        output$manager_section_i <- renderText({
            paste(manager_section_i$name,
                  sanitize_output(
                      manager_section_i$data
                  ),
                  sep = "")
        })
        
        
        # section III ####
        manager_section_iii_conference_domestic <-
            present_table(
                ipcas_db = ipcas_db,
                person_id = loc$dpt_people,
                tbl = "conferences",
                tbl_id = "conference_id",
                filter_col = "conference_location",
                filter_val = "Domácí",
                names_df = names_df_switch("conference_domestic"),
                person_id_selected = input$persons,
                dpt_people = loc$people
            )
        output$manager_section_iii_conference_domestic <- renderText({
            paste(manager_section_iii_conference_domestic$name,
                  sanitize_output(
                      manager_section_iii_conference_domestic$data
                      ),
                  sep = "")
        })
        

        manager_section_iii_conference_foreign <-
            present_table(
                ipcas_db = ipcas_db,
                person_id = loc$dpt_people,
                tbl = "conferences",
                tbl_id = "conference_id",
                filter_col = "conference_location",
                filter_val = "Zahraniční",
                names_df = names_df_switch("conference_domestic"),
                person_id_selected = input$persons,
                dpt_people = loc$people
            )
        output$manager_section_iii_conference_foreign <- renderText({
            paste(manager_section_iii_conference_foreign$name,
                  sanitize_output(
                      manager_section_iii_conference_foreign$data
                  ),
                  sep = "")
        })
        
        # section XI review ####
        
        
        
        manager_section_ix_review <-
            present_table(
                ipcas_db = ipcas_db,
                person_id = loc$dpt_people,
                tbl = "other_reviews",
                tbl_id = "other_reviews_id",
                filter_col = NULL,
                filter_val = NULL,
                names_df = names_df_switch("other_reviews"),
                person_id_selected = input$persons,
                dpt_people = loc$people
            )
        output$manager_section_ix_review <- renderText({
            paste(manager_section_ix_review$name,
                  sanitize_output(
                      manager_section_ix_review$data
                  ),
                  sep = "")
        })
        

    })
    
    # tab control ####

    observeEvent(input$tabs, {
        
        if (input$tabs == "department_view") {
        golem::invoke_js("hide", "#docx_ui_1-download_docx")
        } else {
            golem::invoke_js("show", "#docx_ui_1-download_docx")
        }
            
        })
    
    
  })
}
    
## To be copied in the UI
# mod_manager_ui("manager_1")

## To be copied in the server
# mod_manager_server("manager_1")
