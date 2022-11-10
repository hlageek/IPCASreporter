preview_welcome <- function(i18n) {
    tagList(
    h2(i18n$t("Vítejte v aplikaci IPCASreporter")),
    br(),
    p(i18n$t("Aplikace nyní nahrává Vaše data. Počkejte prosím.")),
    br(),
    p(i18n$t("Před prvním použitím aplikace si přečtěte základní instrukce."))
    )
}


preview_standard <- function(ns, i18n) {
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
        h4(i18n$t("OSOBNÍ ÚDAJE")),
        i18n$t("Jméno:"),
        textOutput(ns("employee_name"), inline = TRUE),
        
        br(),
        i18n$t("Oddělení:"),
        textOutput(ns("department"), inline = TRUE),
        
        br(),
        i18n$t("Úvazek:"),
        textOutput(ns("fte"), inline = TRUE),
        
        br(),
        i18n$t("E-mail:"),
        textOutput(ns("email"), inline = TRUE),
        
        br(),
        i18n$t("Komentář:"),
        textOutput(ns("comment"), inline = TRUE),
        
        
        br(),
        h4(i18n$t("I. VYDANÉ PUBLIKACE")),
        htmlOutput(ns("section_i"), inline = FALSE),
        
        br(),
        h4(i18n$t("II. ORGANIZACE KONFERENCÍ A WORKSHOPŮ")),
        htmlOutput(ns("section_ii"), inline = FALSE),
        
        br(),
        h4(i18n$t("III. PEDAGOGICKÁ A PŘEDNÁŠKOVÁ ČINNOST")),
        h5(i18n$t("1) Výuka na vysokých školách a vedení prací")),
        htmlOutput(ns("section_iii_undergrad"), inline = FALSE),
        htmlOutput(ns("section_iii_postgrad"), inline = FALSE),
        
        
        br(),
        h5(i18n$t("2) Příspěvky a přednášky na konferencích")),
        htmlOutput(ns("section_iii_conference_foreign"), inline = FALSE),
        htmlOutput(ns("section_iii_conference_domestic"), inline = FALSE),
        
        br(),
        h5(i18n$t("3) Samostatné přednášky")),
        htmlOutput(ns("section_iii_lecture_foreign"), inline = FALSE),
        htmlOutput(ns("section_iii_lecture_domestic"), inline = FALSE),
        
        br(),
        h4(i18n$t("IV. ŘEŠENÉ ČI SPOLUŘEŠENÉ GRANTY")),
        h5(i18n$t("Řešené či spoluřešené granty")),
        htmlOutput(ns("section_iv_funded"), inline = FALSE),
        h5(i18n$t("Projekty podané a nepřijaté k financování")),
        htmlOutput(ns("section_iv_unfunded"), inline = FALSE),
        
        br(),
        h4(i18n$t("V. ŘEŠENÉ PROJEKTY V RÁMCI STRATEGIE AV 21")),
        htmlOutput(ns("section_v"), inline = FALSE),
        
        br(),
        h4(i18n$t("VI. POPULARIZAČNÍ ČINNOST")),
        h5(i18n$t("Akce")),
        htmlOutput(ns("section_vi_popular_events"), inline = FALSE),
        h5(i18n$t("Přednášky na středních, případně základních školách")),
        htmlOutput(ns("section_vi_school_events"), inline = FALSE),
        h5(i18n$t("Vystoupení a popularizační texty v médiích")),
        htmlOutput(ns("section_vi_media"), inline = FALSE),
        
        
        br(),
        h4(i18n$t("VII. SPOLUPRÁCE SE STÁTNÍ A VEŘEJNOU SPRÁVOU")),
        htmlOutput(ns("section_vii"), inline = FALSE),
        
        br(),
        h4(i18n$t("VIII. ZAHRANIČNÍ SPOLUPRÁCE")),
        h5(i18n$t("Zapojení do mezinárodních projektů")),
        htmlOutput(ns("section_viii_int_projects"), inline = FALSE),
        h5(i18n$t("Mezinárodní dvoustranné dohody")),
        htmlOutput(ns("section_viii_int_bilateral"), inline = FALSE),
        
        br(),
        h4(i18n$t("IX. OSTATNÍ")),
        h5(i18n$t("Ocenění odbornou komunitou")),
        htmlOutput(ns("section_ix_award"), inline = FALSE),
        h5(i18n$t("Posudky")),
        htmlOutput(ns("section_ix_review"), inline = FALSE),
        h5(i18n$t("Odborná grémia, redakční a oborové rady apod.")),
        h6(i18n$t("Domácí")),
        htmlOutput(ns("section_ix_member_domestic"), inline = FALSE),
        h6(i18n$t("Zahraniční")),
        htmlOutput(ns("section_ix_member_foreign"), inline = FALSE),
        h5(i18n$t("Redakční práce")),
        htmlOutput(ns("section_ix_editions"), inline = FALSE),
        
        br(),
        h4(i18n$t("X. ROZPRACOVANÉ PUBLIKACE A PROJEKTY")),
        htmlOutput(ns("section_x"), inline = FALSE),
        
        br(),
        h4(i18n$t("XI. RŮZNÉ")),
        htmlOutput(ns("section_xi"), inline = FALSE)
    ))
}