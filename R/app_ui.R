
#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
    i18n <- golem::get_golem_options(which = "translator")

    
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shiny.i18n::usei18n(i18n),
    
    # List the first level UI elements here
    fluidPage(

              theme = shinythemes::shinytheme("journal"),
      titlePanel(title = img(src="https://www.flu.cas.cz/images/logo_web_prehozene_krivky_50.png"), "IP CAS annual report"),
      tags$div(
      selectInput(inputId = "lang",
                  label = NULL,
                  choices = i18n$get_languages()
                  ),
      style = "float: right; max-width: 70px"),

                     navlistPanel(widths = c(2,10), well = F, #####

                            tabPanel(i18n$t("NÁHLED"),

                                     mod_docx_ui("docx_ui_1",  i18n),


                                     mod_preview_ui("preview_ui_1", i18n)

                            ),


                            tabPanel(i18n$t("OSOBNÍ ÚDAJE"),

                                     h2(i18n$t("OSOBNÍ ÚDAJE")),
                                     mod_identification_ui("identification_ui_1", i18n)

                                     ),

                            tabPanel(i18n$t("I. VYDANÉ PUBLIKACE"),

                                     h2(i18n$t("I. VYDANÉ PUBLIKACE")),
                                     p(i18n$t("Včetně odkazu do ASEP.")),

                                     mod_pub_ui("pub_ui_1", i18n)

                                     ),

                            tabPanel(i18n$t("II. ORGANIZACE KONFERENCÍ A WORKSHOPŮ"),

                                     h2(i18n$t("II. ORGANIZACE KONFERENCÍ A WORKSHOPŮ")),
                                     p( "Including ASEP reference."),

                                     mod_events_ui("events_ui_1", i18n)

                                     ),

                            tabPanel(i18n$t("III. PEDAGOGICKÁ A PŘEDNÁŠKOVÁ ČINNOST"),

                                     h2(i18n$t("III. PEDAGOGICKÁ A PŘEDNÁŠKOVÁ ČINNOST")),

                                     p("Neuvádějte pedagogické aktivity nesouvisející s Vaším odborným působením ve FLU a vycházející např. z vedlejšího prac. poměru na VŠ."),

                                     tabsetPanel(tabPanel("Bc. & Mgr.",

                                     mod_undergrad_ui("undergrad_ui_1", i18n),
                                     br(),br(),br(),br(),br(),
                                     ),
                                     tabPanel("PhD.",

                                     mod_postgrad_ui("postgrad_ui_1", i18n),
                                     br(),br(),br(),br(),br(),
                                     ),

                                     tabPanel("Konference",

                                     mod_conference_ui("conference_ui_1", i18n),
                                     br(),br(),br(),br(),br(),
                                     ),
                                     tabPanel("Přednášky",

                                     mod_lectures_ui("lectures_ui_1", i18n),
                                     br(),br(),br(),br(),br(),
                                     )
                                     )
                                     ),

                            tabPanel(i18n$t("IV. ŘEŠENÉ ČI SPOLUŘEŠENÉ GRANTY"),

                                     h2(i18n$t("IV. ŘEŠENÉ ČI SPOLUŘEŠENÉ GRANTY")),
                                     p("Uveďte i projekty podané a nepřijaté k financování."),

                                     mod_grants_ui("grants_ui_1", i18n)


                                     ),


                            tabPanel(i18n$t("V. ŘEŠENÉ PROJEKTY V RÁMCI STRATEGIE AV 21"),

                                     h2(i18n$t("V. ŘEŠENÉ PROJEKTY V RÁMCI STRATEGIE AV 21")),
                                     p("Včetně anotace (min. 300 znaků) a výstupů (publikace, konference, přednáška atd.)."),

                                     mod_av21_ui("av21_ui_1", i18n)


                                     ),
                            tabPanel(i18n$t("VI. POPULARIZAČNÍ ČINNOST"),
                                     h2(i18n$t("VI. POPULARIZAČNÍ ČINNOST")),
                                     p("(Příklad: název akce: Týden vědy a techniky, popis aktivity: odborná přednáška; popularizační přednáška; čtení atd.)"),

                                     tabsetPanel(

                                       tabPanel("Akce",

                                                mod_popular_ui("popular_ui_1", i18n)
                                                ),

                                       tabPanel("Přednášky na středních, případně základních školách",

                                                mod_school_ui("school_ui_1", i18n)
                                       ),

                                       tabPanel("Vystoupení a popularizační texty v médiích",

                                                mod_media_ui("media_ui_1", i18n)

                                                )

                                     )
                            ),
                            tabPanel(i18n$t("VII. SPOLUPRÁCE SE STÁTNÍ A VEŘEJNOU SPRÁVOU"),
                                     h2(i18n$t("VII. SPOLUPRÁCE SE STÁTNÍ A VEŘEJNOU SPRÁVOU")),
                                     p("(Např. členství v panelu GAČR; hodnocení pro RIV; hodnocení pro RVVI; odborné expertizy a vědecké poradenství – s uvedením zadavatele apod.)."),

                                     mod_public_ui("public_ui_1", i18n)

                                     ),
                            tabPanel(i18n$t("VIII. ZAHRANIČNÍ SPOLUPRÁCE"),
                                     h2(i18n$t("VIII. ZAHRANIČNÍ SPOLUPRÁCE")),

                                     tabsetPanel(

                                       tabPanel("Zapojení do mezinárodních projektů",

                                                mod_int_projects_ui("int_projects_ui_1", i18n)
                                       ),

                                       tabPanel("Mezinárodní dvoustranné dohody",

                                                mod_int_bilateral_ui("int_bilateral_ui_1", i18n)
                                       )

                                     )


                                     ),
                            tabPanel(i18n$t("IX. OSTATNÍ"),
                                     h2(i18n$t("IX. OSTATNÍ")),

                                     tabsetPanel(

                                       tabPanel("Ocenění odbornou komunitou",

                                                mod_other_award_ui("other_award_ui_1", i18n)
                                       ),

                                       tabPanel("Posudky",
                                                p("Posudky článků pro časopisy, knih pro nakladatelství (počet posuzovaných stran), diplomových a doktorských prací (počet posuzovaných stran), grantů a projektů"),

                                                mod_other_review_ui("other_review_ui_1", i18n)
                                       ),

                                       tabPanel("Odborná grémia, redakční a oborové rady apod.",

                                                mod_other_member_ui("other_member_ui_1", i18n)

                                       ),

                                       tabPanel("Redakční práce",

                                                mod_other_editions_ui("other_editions_ui_1")

                                       )
                                     )


                                     ),
                            tabPanel(i18n$t("X. ROZPRACOVANÉ PUBLIKACE A PROJEKTY"),
                                     h2(i18n$t("X. ROZPRACOVANÉ PUBLIKACE A PROJEKTY")),
                                     p("Uveďte prosím analogicky podle části I, udejte množství v normostranách [1 normostrana - 1800 znaků vč. mezer], které bylo vypracováno v tomto období; uveďte a specifikujte rovněž případnou práci na databázích"),

                                     mod_wip_ui("wip_ui_1")



                                     ),
                            tabPanel(i18n$t("XI. RŮZNÉ"),
                                     h2(i18n$t("XI. RŮZNÉ")),
                                     p("Uveďte vše další, co pokládáte za důležité a relevantní pro Vaši práci v daném období."),
                                     mod_various_ui("various_ui_1")
                                     )


               ),





    ),

  )

}



#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'IPCASreporter'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
