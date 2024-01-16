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
  
fluidRow(
    
    column(10,
    
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
                  numericInput(ns("year"),
                               label = i18n$t("Rok"),
                               value = as.integer( format(Sys.Date(), "%Y")),
                               min = as.integer( format(Sys.Date(), "%Y"))-1,
                               max = as.integer( format(Sys.Date(), "%Y"))
                   ),
                   
                   h4(i18n$t("I. VYDANÉ PUBLIKACE")),
                   htmlOutput(ns("manager_section_i"), inline = FALSE),
                   
                   br(),
                   h4(i18n$t("II. ORGANIZACE KONFERENCÍ A WORKSHOPŮ")),
                   htmlOutput(ns("manager_section_ii"), inline = FALSE),
                   
                   br(),
                   h5(i18n$t("III. PEDAGOGICKÁ A PŘEDNÁŠKOVÁ ČINNOST")),
                   h5(i18n$t("1) Výuka na vysokých školách a vedení prací")),
                   h5(i18n$t("a) Bakalářské a magisterské studijní programy")),
                   htmlOutput(ns("manager_section_iii_undergrad"), inline = FALSE),
                   h5(i18n$t("b) Doktorský studijní program")),
                   htmlOutput(ns("manager_section_iii_postgrad"), inline = FALSE),
                   
                   
                   br(),
                   h5(i18n$t("2) Příspěvky a přednášky na konferencích")),
                   h5(i18n$t("Zahraniční")),
                   htmlOutput(ns("manager_section_iii_conference_foreign"), inline = FALSE),
                   h5(i18n$t("Domácí")),
                   htmlOutput(ns("manager_section_iii_conference_domestic"), inline = FALSE),
                   
                   br(),
                   h5(i18n$t("3) Samostatné přednášky")),
                   h5(i18n$t("Zahraniční")),
                   htmlOutput(ns("manager_section_iii_lecture_foreign"), inline = FALSE),
                   h5(i18n$t("Domácí")),
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
      )),
    column(2, 
    uiOutput(ns("section_selector"))
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


    # filter for dpt heads ####
    observeEvent(usr, { 
    
    if (req(usr$level) < 3) {

    department_limited <- golem::get_golem_options(which = "department_heads") %>% 
    dplyr::filter(head_id == usr$person_id) %>% 
    dplyr::pull(department_name)

    if (length(department_limited) < 1) {
        department_limited <- ""
    }

    updateSelectInput(session = session,
                              "department", 
                              selected = department_limited,
                              choices = department_limited)
            
    }

    })
    
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
        
        # output$section_selector <- renderUI({
        #     selectInput("sections", "Sekce", choices = c(
        #         "I. VYDANÉ PUBLIKACE" = 1,
        #         "II. ORGANIZACE KONFERENCÍ A WORKSHOPŮ" = 2,
        #         "III. PEDAGOGICKÁ A PŘEDNÁŠKOVÁ ČINNOST" = 3,
        #         "IV. ŘEŠENÉ ČI SPOLUŘEŠENÉ GRANTY" = 4,
        #         "V. ŘEŠENÉ PROJEKTY V RÁMCI STRATEGIE AV 21" = 5,
        #         "VI. POPULARIZAČNÍ ČINNOST" = 6,
        #         "VII. SPOLUPRÁCE SE STÁTNÍ A VEŘEJNOU SPRÁVOU" = 7,
        #         "VIII. ZAHRANIČNÍ SPOLUPRÁCE" = 8,
        #         "IX. OSTATNÍ" = 9,
        #         "X. ROZPRACOVANÉ PUBLIKACE A PROJEKTY" = 10,
        #         "XI. RŮZNÉ" = 11
        #     ),
        #     selected = seq(1,11),
        #     selectize = FALSE,
        #     multiple = TRUE,
        #     size = 11) %>% tagAppendAttributes(style = "margin-top: Opx; float:right;")
        # })
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
                dpt_people = loc$people, year = input$year
            )
        output$manager_section_i <- renderText({
            paste(manager_section_i$name,
                  sanitize_output(
                      manager_section_i$data
                  ),
                  sep = "")
        })
        
        # section II ####
        
        manager_section_ii <- 
            present_table(
                ipcas_db = ipcas_db,
                person_id = loc$dpt_people,
                tbl = "events",
                tbl_id = "event_id",
                filter_col = NULL,
                filter_val = NULL,
                names_df = names_df_switch("events"),
                person_id_selected = input$persons,
                dpt_people = loc$people, year = input$year
            )
        output$manager_section_ii <- renderText({
            paste(manager_section_ii$name,
                  sanitize_output(
                      manager_section_ii$data
                  ),
                  sep = "")
        })
        
        # section III  undergrad ####
        
        manager_section_iii_undergrad <-
            present_table(
                ipcas_db = ipcas_db,
                person_id = loc$dpt_people,
                tbl = "undergrad",
                tbl_id = "undergrad_id",
                filter_col = NULL,
                filter_val = NULL,
                names_df = names_df_switch("undergrad"),
                person_id_selected = input$persons,
                dpt_people = loc$people, year = input$year
            )
        output$manager_section_iii_undergrad <- renderText({
            paste(manager_section_iii_undergrad$name,
                  sanitize_output(
                      manager_section_iii_undergrad$data
                  ),
                  sep = "")
        })
        
        # section III  postgrad ####
        
        manager_section_iii_postgrad <-
            present_table(
                ipcas_db = ipcas_db,
                person_id = loc$dpt_people,
                tbl = "postgrad",
                tbl_id = "postgrad_id",
                filter_col = NULL,
                filter_val = NULL,
                names_df = names_df_switch("postgrad"),
                person_id_selected = input$persons,
                dpt_people = loc$people, year = input$year
            )
        output$manager_section_iii_postgrad <- renderText({
            paste(manager_section_iii_postgrad$name,
                  sanitize_output(
                      manager_section_iii_postgrad$data
                  ),
                  sep = "")
        })
        
        # section III  conference_domestic ####
        manager_section_iii_conference_domestic <-
            present_table(
                ipcas_db = ipcas_db,
                person_id = loc$dpt_people,
                tbl = "conferences",
                tbl_id = "conference_id",
                filter_col = "conference_location",
                filter_val = "Domácí",
                names_df = names_df_switch("conference"),
                person_id_selected = input$persons,
                dpt_people = loc$people, year = input$year
            )
        output$manager_section_iii_conference_domestic <- renderText({
            paste(manager_section_iii_conference_domestic$name,
                  sanitize_output(
                      manager_section_iii_conference_domestic$data
                      ),
                  sep = "")
        })
        
        # section III  conference_foreign ####
        manager_section_iii_conference_foreign <-
            present_table(
                ipcas_db = ipcas_db,
                person_id = loc$dpt_people,
                tbl = "conferences",
                tbl_id = "conference_id",
                filter_col = "conference_location",
                filter_val = "Zahraniční",
                names_df = names_df_switch("conference"),
                person_id_selected = input$persons,
                dpt_people = loc$people, year = input$year
            )
        output$manager_section_iii_conference_foreign <- renderText({
            paste(manager_section_iii_conference_foreign$name,
                  sanitize_output(
                      manager_section_iii_conference_foreign$data
                  ),
                  sep = "")
        })
        
        # section III  lectures_foreign ####
        
        manager_section_iii_lecture_foreign <-
            present_table(
                ipcas_db = ipcas_db,
                person_id = loc$dpt_people,
                tbl = "lectures",
                tbl_id = "lecture_id",
                filter_col = "lecture_location",
                filter_val = "Zahraniční",
                names_df = names_df_switch("lecture"),
                person_id_selected = input$persons,
                dpt_people = loc$people, year = input$year
            )
        output$manager_section_iii_lecture_foreign <- renderText({
            paste(manager_section_iii_lecture_foreign$name,
                  sanitize_output(
                      manager_section_iii_lecture_foreign$data
                  ),
                  sep = "")
        })
        
        # section III  lectures_domestic ####
        
        manager_section_iii_lecture_domestic <-
            present_table(
                ipcas_db = ipcas_db,
                person_id = loc$dpt_people,
                tbl = "lectures",
                tbl_id = "lecture_id",
                filter_col = "lecture_location",
                filter_val = "Domácí",
                names_df = names_df_switch("lecture"),
                person_id_selected = input$persons,
                dpt_people = loc$people, year = input$year
            )
        output$manager_section_iii_lecture_domestic <- renderText({
            paste(manager_section_iii_lecture_domestic$name,
                  sanitize_output(
                      manager_section_iii_lecture_domestic$data
                  ),
                  sep = "")
        })
        
        
        # section IV  funded ####
        
        manager_section_iv_funded <-
            present_table(
                ipcas_db = ipcas_db,
                person_id = loc$dpt_people,
                tbl = "grants",
                tbl_id = "grant_id",
                filter_col = "grant_funding_status",
                filter_val = "funded",
                names_df = names_df_switch("grant"),
                person_id_selected = input$persons,
                dpt_people = loc$people, year = input$year
            )
        output$manager_section_iv_funded <- renderText({
            paste(manager_section_iv_funded$name,
                  sanitize_output(
                      manager_section_iv_funded$data
                  ),
                  sep = "")
        })
        
        
        # section IV  unfunded ####
        
        manager_section_iv_unfunded <-
            present_table(
                ipcas_db = ipcas_db,
                person_id = loc$dpt_people,
                tbl = "grants",
                tbl_id = "grant_id",
                filter_col = "grant_funding_status",
                filter_val = "unfunded",
                names_df = names_df_switch("grant"),
                person_id_selected = input$persons,
                dpt_people = loc$people, year = input$year
            )
        output$manager_section_iv_unfunded <- renderText({
            paste(manager_section_iv_unfunded$name,
                  sanitize_output(
                      manager_section_iv_unfunded$data
                  ),
                  sep = "")
        })
        
        # section V  av21 ####
        
        manager_section_v <-
            present_table(
                ipcas_db = ipcas_db,
                person_id = loc$dpt_people,
                tbl = "av21",
                tbl_id = "av21_id",
                filter_col = NULL,
                filter_val = NULL,
                names_df = names_df_switch("av21"),
                person_id_selected = input$persons,
                dpt_people = loc$people, year = input$year
            )
        output$manager_section_v <- renderText({
            paste(manager_section_v$name,
                  sanitize_output(
                      manager_section_v$data
                  ),
                  sep = "")
        })
        
        # section VI popular ####
        
        manager_section_vi_popular_events <-
            present_table(
                ipcas_db = ipcas_db,
                person_id = loc$dpt_people,
                tbl = "popular",
                tbl_id = "popular_id",
                filter_col = NULL,
                filter_val = NULL,
                names_df = names_df_switch("popular"),
                person_id_selected = input$persons,
                dpt_people = loc$people, year = input$year
            )
        output$manager_section_vi_popular_events <- renderText({
            paste(manager_section_vi_popular_events$name,
                  sanitize_output(
                      manager_section_vi_popular_events$data
                  ),
                  sep = "")
        })
        
        # section VI school ####
        
        manager_section_vi_school_events <-
            present_table(
                ipcas_db = ipcas_db,
                person_id = loc$dpt_people,
                tbl = "school",
                tbl_id = "school_id",
                filter_col = NULL,
                filter_val = NULL,
                names_df = names_df_switch("school"),
                person_id_selected = input$persons,
                dpt_people = loc$people, year = input$year
            )
        output$manager_section_vi_school_events <- renderText({
            paste(manager_section_vi_school_events$name,
                  sanitize_output(
                      manager_section_vi_school_events$data
                  ),
                  sep = "")
        })
        
        # section VI media ####
        
        manager_section_vi_media <-
            present_table(
                ipcas_db = ipcas_db,
                person_id = loc$dpt_people,
                tbl = "media",
                tbl_id = "media_id",
                filter_col = NULL,
                filter_val = NULL,
                names_df = names_df_switch("media"),
                person_id_selected = input$persons,
                dpt_people = loc$people, year = input$year
            )
        output$manager_section_vi_media <- renderText({
            paste(manager_section_vi_media$name,
                  sanitize_output(
                      manager_section_vi_media$data
                  ),
                  sep = "")
        })
        
        # section VII gov ####
        
        manager_section_vii <-
            present_table(
                ipcas_db = ipcas_db,
                person_id = loc$dpt_people,
                tbl = "gov",
                tbl_id = "gov_id",
                filter_col = NULL,
                filter_val = NULL,
                names_df = names_df_switch("gov"),
                person_id_selected = input$persons,
                dpt_people = loc$people, year = input$year
            )
        output$manager_section_vii <- renderText({
            paste(manager_section_vii$name,
                  sanitize_output(
                      manager_section_vii$data
                  ),
                  sep = "")
        })
        
        # section VIII int ####
        
        manager_section_viii_int_projects <-
            present_table(
                ipcas_db = ipcas_db,
                person_id = loc$dpt_people,
                tbl = "int_projects",
                tbl_id = "int_projects_id",
                filter_col = NULL,
                filter_val = NULL,
                names_df = names_df_switch("int_projects"),
                person_id_selected = input$persons,
                dpt_people = loc$people, year = input$year
            )
        output$manager_section_viii_int_projects <- renderText({
            paste(manager_section_viii_int_projects$name,
                  sanitize_output(
                      manager_section_viii_int_projects$data
                  ),
                  sep = "")
        })
        
        # section VIII bilateral ####
        
        manager_section_viii_int_bilateral <-
            present_table(
                ipcas_db = ipcas_db,
                person_id = loc$dpt_people,
                tbl = "int_bilateral",
                tbl_id = "int_bilateral_id",
                filter_col = NULL,
                filter_val = NULL,
                names_df = names_df_switch("int_bilateral"),
                person_id_selected = input$persons,
                dpt_people = loc$people, year = input$year
            )
        output$manager_section_viii_int_bilateral <- renderText({
            paste(manager_section_viii_int_bilateral$name,
                  sanitize_output(
                      manager_section_viii_int_bilateral$data
                  ),
                  sep = "")
        })
        
        
        
        # section IX award ####
        
        manager_section_ix_award <-
            present_table(
                ipcas_db = ipcas_db,
                person_id = loc$dpt_people,
                tbl = "other_awards",
                tbl_id = "other_awards_id",
                filter_col = NULL,
                filter_val = NULL,
                names_df = names_df_switch("other_awards"),
                person_id_selected = input$persons,
                dpt_people = loc$people, year = input$year
            )
        output$manager_section_ix_award <- renderText({
            paste(manager_section_ix_award$name,
                  sanitize_output(
                      manager_section_ix_award$data
                  ),
                  sep = "")
        })
        
        # section IX review ####
        
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
                dpt_people = loc$people, year = input$year
            )
        output$manager_section_ix_review <- renderText({
            paste(manager_section_ix_review$name,
                  sanitize_output(
                      manager_section_ix_review$data
                  ),
                  sep = "")
        })
        
        # section IX member domestic ####
        
        manager_section_ix_member_domestic <-
            present_table(
                ipcas_db = ipcas_db,
                person_id = loc$dpt_people,
                tbl = "other_member",
                tbl_id = "other_member_id",
                filter_col = "other_member_location",
                filter_val = "Domácí",
                names_df = names_df_switch("other_member"),
                person_id_selected = input$persons,
                dpt_people = loc$people, year = input$year
            )
        output$manager_section_ix_member_domestic <- renderText({
            paste(manager_section_ix_member_domestic$name,
                  sanitize_output(
                      manager_section_ix_member_domestic$data
                  ),
                  sep = "")
        })
        
        
        # section IX member foreign ####
        
        manager_section_ix_member_foreign <-
            present_table(
                ipcas_db = ipcas_db,
                person_id = loc$dpt_people,
                tbl = "other_member",
                tbl_id = "other_member_id",
                filter_col = "other_member_location",
                filter_val = "Zahraniční",
                names_df = names_df_switch("other_member"),
                person_id_selected = input$persons,
                dpt_people = loc$people, year = input$year
            )
        output$manager_section_ix_member_foreign <- renderText({
            paste(manager_section_ix_member_foreign$name,
                  sanitize_output(
                      manager_section_ix_member_foreign$data
                  ),
                  sep = "")
        })
        
        
        # section IX  editions ####
        
        manager_section_ix_member_foreign <-
            present_table(
                ipcas_db = ipcas_db,
                person_id = loc$dpt_people,
                tbl = "other_editions",
                tbl_id = "other_editions_id",
                filter_col = NULL,
                filter_val = NULL,
                names_df = names_df_switch("other_editions"),
                person_id_selected = input$persons,
                dpt_people = loc$people, year = input$year
            )
        output$manager_section_ix_member_foreign <- renderText({
            paste(manager_section_ix_member_foreign$name,
                  sanitize_output(
                      manager_section_ix_member_foreign$data
                  ),
                  sep = "")
        })
        
        # section X  wip ####
        
        manager_section_x <-
            present_table(
                ipcas_db = ipcas_db,
                person_id = loc$dpt_people,
                tbl = "wip",
                tbl_id = "wip_id",
                filter_col = NULL,
                filter_val = NULL,
                names_df = names_df_switch("wip"),
                person_id_selected = input$persons,
                dpt_people = loc$people, year = input$year
            )
        output$manager_section_x <- renderText({
            paste(manager_section_x$name,
                  sanitize_output(
                      manager_section_x$data
                  ),
                  sep = "")
        })
        
        # section XI various ####
        
        manager_section_xi <-
            present_table(
                ipcas_db = ipcas_db,
                person_id = loc$dpt_people,
                tbl = "various",
                tbl_id = "various_id",
                filter_col = NULL,
                filter_val = NULL,
                names_df = names_df_switch("various"),
                person_id_selected = input$persons,
                dpt_people = loc$people, year = input$year
            )
        output$manager_section_xi <- renderText({
            paste(manager_section_xi$name,
                  sanitize_output(
                      manager_section_xi$data
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
