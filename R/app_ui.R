
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
      style = "float: right; max-width: 60px"),
                     navlistPanel(id = "sections_panel", widths = c(2,10), well = F, #####

                            tabPanel(i18n$t("NÁHLED"),
                                     
                                     fluidRow(
                                     tags$div(
                                         mod_docx_ui("docx_ui_1",  i18n),
                                         style = "float: right; margin-right: 15px")
                                     ),

                                     fluidRow(
                                         uiOutput("deadline"),
                                         mod_preview_ui("preview_ui_1", i18n)
                                         )

                                    

                            ),


                            tabPanel(i18n$t("OSOBNÍ ÚDAJE"),

                                     h2(i18n$t("OSOBNÍ ÚDAJE")),
                                     mod_identification_ui("identification_ui_1", i18n)

                                     ),

                            tabPanel(i18n$t("I. VYDANÉ PUBLIKACE"), value = "section1",

                                     h2(i18n$t("I. VYDANÉ PUBLIKACE")),
                                     p(i18n$t("Všechny uvedené publikace a výstupy musí být zaneseny do databáze"),
                                       " ",
                                       tags$a(href="https://asep.lib.cas.cz/arl-cav/cs/index/", "ASEP", target="_blank"),
                                       " ",
                                       i18n$t("a obsahovat odkaz na záznam. Uveďte rovněž ty publikace a výstupy, které vyšly koncem loňského roku a které jste v loňském výkazu neuváděli.")),

                                     mod_pub_ui("pub_ui_1", i18n)

                                     ) |> tagAppendAttributes(class = "section"),

                            tabPanel(i18n$t("II. ORGANIZACE KONFERENCÍ A WORKSHOPŮ"),  value = "section2",

                                     h2(i18n$t("II. ORGANIZACE KONFERENCÍ A WORKSHOPŮ")),
                                     p(i18n$t("Všechny uvedené odborné akce musí být zaneseny do databáze"),
                                       " ",
                                       tags$a(href="https://asep.lib.cas.cz/arl-cav/cs/index/", "ASEP", target="_blank"),
                                       " ",
                                       i18n$t("a obsahovat odkaz na záznam. Uveďte rovněž akce organizované koncem loňského roku, které jste v loňském výkazu neuváděli.")),

                                     mod_events_ui("events_ui_1", i18n)

                                     ) |> tagAppendAttributes(class = "section"),

                            tabPanel(i18n$t("III. PEDAGOGICKÁ A PŘEDNÁŠKOVÁ ČINNOST"),  value = "section3",

                                     h2(i18n$t("III. PEDAGOGICKÁ A PŘEDNÁŠKOVÁ ČINNOST")),

                                     p(i18n$t("Neuvádějte pedagogické aktivity nesouvisející s Vaším odborným působením ve FLU a vycházející např. z vedlejšího prac. poměru na VŠ.")),

                                     tabsetPanel(
                                    
                                    tabPanel(i18n$t("Bc. & Mgr."),

                                     mod_undergrad_ui("undergrad_ui_1", i18n),
                                     br(),br(),br(),br(),br(),
                                     ),
                                    
                                     tabPanel(i18n$t("PhD."),

                                     mod_postgrad_ui("postgrad_ui_1", i18n),
                                     br(),br(),br(),br(),br(),
                                     ),

                                     tabPanel(i18n$t("Konference"),

                                     mod_conference_ui("conference_ui_1", i18n),
                                     br(),br(),br(),br(),br(),
                                     ),
                                     tabPanel(i18n$t("Přednášky"),

                                     mod_lectures_ui("lectures_ui_1", i18n),
                                     br(),br(),br(),br(),br(),
                                     )
                                     )
                                     ) |> tagAppendAttributes(class = "section"),

                            tabPanel(i18n$t("IV. ŘEŠENÉ ČI SPOLUŘEŠENÉ GRANTY"),  value = "section4",

                                     h2(i18n$t("IV. ŘEŠENÉ ČI SPOLUŘEŠENÉ GRANTY")),
                                     p(i18n$t("Uveďte i projekty podané a nepřijaté k financování.")),

                                     mod_grants_ui("grants_ui_1", i18n)


                                     ) |> tagAppendAttributes(class = "section"),


                            tabPanel(i18n$t("V. ŘEŠENÉ PROJEKTY V RÁMCI STRATEGIE AV 21"),  value = "section5",

                                     h2(i18n$t("V. ŘEŠENÉ PROJEKTY V RÁMCI STRATEGIE AV 21")),
                                     p(i18n$t("Včetně anotace (min. 300 znaků) a výstupů (publikace, konference, přednáška atd.).")),

                                     mod_av21_ui("av21_ui_1", i18n)


                                     ) |> tagAppendAttributes(class = "section"),
                            tabPanel(i18n$t("VI. POPULARIZAČNÍ ČINNOST"),  value = "section6",
                                     h2(i18n$t("VI. POPULARIZAČNÍ ČINNOST")),
                                     p(i18n$t("(Příklad: název akce: Týden vědy a techniky, popis aktivity: odborná přednáška; popularizační přednáška; čtení atd.)")),

                                     tabsetPanel(

                                       tabPanel(i18n$t("Akce"),

                                                mod_popular_ui("popular_ui_1", i18n)
                                                ),

                                       tabPanel(i18n$t("Přednášky na středních, případně základních školách"),

                                                mod_school_ui("school_ui_1", i18n)
                                       ),

                                       tabPanel(i18n$t("Vystoupení a popularizační texty v médiích"),

                                                mod_media_ui("media_ui_1", i18n)

                                                )

                                     )
                            ) |> tagAppendAttributes(class = "section"),
                            tabPanel(i18n$t("VII. SPOLUPRÁCE SE STÁTNÍ A VEŘEJNOU SPRÁVOU"),  value = "section7",
                                     h2(i18n$t("VII. SPOLUPRÁCE SE STÁTNÍ A VEŘEJNOU SPRÁVOU")),
                                     p(i18n$t("(Např. členství v panelu GAČR; hodnocení pro RIV; hodnocení pro RVVI; odborné expertizy a vědecké poradenství – s uvedením zadavatele apod.).")),

                                     mod_public_ui("public_ui_1", i18n)

                                     ) |> tagAppendAttributes(class = "section"),
                            tabPanel(i18n$t("VIII. ZAHRANIČNÍ SPOLUPRÁCE"),  value = "section8",
                                     h2(i18n$t("VIII. ZAHRANIČNÍ SPOLUPRÁCE")),

                                     tabsetPanel(

                                       tabPanel(i18n$t("Zapojení do mezinárodních projektů"),

                                                mod_int_projects_ui("int_projects_ui_1", i18n)
                                       ),

                                       tabPanel(i18n$t("Mezinárodní dvoustranné dohody"),

                                                mod_int_bilateral_ui("int_bilateral_ui_1", i18n)
                                       )

                                     )


                                     ) |> tagAppendAttributes(class = "section"),
                            tabPanel(i18n$t("IX. OSTATNÍ"),  value = "section9",
                                     h2(i18n$t("IX. OSTATNÍ")),

                                     tabsetPanel(

                                       tabPanel(i18n$t("Ocenění odbornou komunitou"),

                                                mod_other_award_ui("other_award_ui_1", i18n)
                                       ),

                                       tabPanel(i18n$t("Posudky"),
                                                p(i18n$t("Posudky článků pro časopisy, knih pro nakladatelství (počet posuzovaných stran), diplomových a doktorských prací (počet posuzovaných stran), grantů a projektů")),

                                                mod_other_review_ui("other_review_ui_1", i18n)
                                       ),

                                       tabPanel(i18n$t("Odborná grémia, redakční a oborové rady apod."),

                                                mod_other_member_ui("other_member_ui_1", i18n)

                                       ),

                                       tabPanel(i18n$t("Redakční práce"),

                                                mod_other_editions_ui("other_editions_ui_1", i18n)

                                       )
                                     )


                                     ) |> tagAppendAttributes(class = "section"),
                            tabPanel(i18n$t("X. ROZPRACOVANÉ PUBLIKACE A PROJEKTY"),  value = "section10",
                                     h2(i18n$t("X. ROZPRACOVANÉ PUBLIKACE A PROJEKTY")),
                                     p(i18n$t("Uveďte prosím analogicky podle části I, udejte množství v normostranách [1 normostrana - 1800 znaků vč. mezer], které bylo vypracováno v tomto období; uveďte a specifikujte rovněž případnou práci na databázích")),

                                     mod_wip_ui("wip_ui_1", i18n)



                                     ) |> tagAppendAttributes(class = "section"),
                            tabPanel(i18n$t("XI. RŮZNÉ"),  value = "section11",
                                     h2(i18n$t("XI. RŮZNÉ")),
                                     p(i18n$t("Uveďte vše další, co pokládáte za důležité a relevantní pro Vaši práci v daném období.")),
                                     mod_various_ui("various_ui_1", i18n)
                                     ) |> tagAppendAttributes(class = "section")


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
