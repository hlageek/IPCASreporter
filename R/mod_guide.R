#' guide UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_guide_ui <- function(id, i18n){
  ns <- NS(id)
  tagList(
    
    actionButton(ns("show_guide"),
      label = i18n$t("Instrukce"),
      icon = icon("info-circle", verify_fa = FALSE))
 
  )
}
    
#' guide Server Function
#'
#' @noRd 
mod_guide_server <- function(id, i18n) {
  moduleServer(id, function(input, output, session) {
    
      ns <- NS(id)

    observeEvent(input$show_guide, {
    showModal(modalDialog(
      size = "l",
      footer = tagList(
        modalButton(i18n()$t("Zavřít"))),
      easyClose = TRUE,
      
      tagList(
        
        h2(i18n()$t("O aplikaci")),
        
        p(
            i18n()$t("Pracovní výkaz slouží mj. ke sběru dat k výroční zprávě FLÚ, dále jako podklad pro návrhy v oblasti odměnovaní zaměstnanců, konkrétně pro stanovení odměn za mimořádný pracovní výkon v daném kalendářním roce a určení výše osobního příplatku pro nový kalendářní rok, event. k atualizaci údajů na webové stránce oddělení."), 
            tags$b(
                i18n()$t("Do výkazu zanášejte jen ty výsledky, které nevykazujete na jiném pracovišti.")
                )
            ),
        
        h2(i18n()$t("Instrukce")),
        
        tags$li(i18n()$t("Vaše jméno bude použito pro vyhledávání v úložišti ASEP, aby bylo možné vybrat možnosti v sekcích"), tags$b( "I. & II.")),
        
        tags$li(i18n()$t("Pomocí nabídky vlevo se můžete pohybovat v aplikaci a pokračovat ve vyplňování všech"), tags$b(i18n()$t("sekcí výkazu."))),
        
        # tags$li(i18n()$t("Použijte tlačítko"), tags$b(i18n()$t("Uložit")), i18n()$t("pro vygenerování odkazu, který obnoví Vaši práci na zprávě, pokud se k ní budete chtít později vrátit.")),
        
        tags$li(i18n()$t("Použijte tlačítko"), tags$b(i18n()$t("Stáhnout")),i18n()$t("pro vygenerování verze zprávy ve formátu MS Word.")),
        
        #tags$li(i18n()$t("Použijte tlačítko"), tags$b(i18n()$t("Odeslat")), i18n()$t("pro odeslání zprávy. Po odeslání obdržíte potvrzovací e-mail.")),
        
        #tags$li(HTML("<i class='fa fa-warning'></i>"), i18n()$t("Aby nedošlo ke ztrátě dat, neobnovujte během používání aplikace prohlížeč!"), style = "color:red"),
        
        h3(i18n()$t("Navigace")),
        
        img(src="https://owncloud.cesnet.cz/index.php/s/sEf5XIUg7d0jNKq/download",
            width="100%"),
        
        tags$ol(
          tags$li(tags$b(i18n()$t("Vertikální navigace")), i18n()$t("pro aplikaci a přepínání mezi"), tags$b(i18n()$t("Náhledem")), i18n()$t("a"), tags$b(i18n()$t("dalšími sekcemi výkazu")), "."),
          
          tags$li(tags$b(i18n()$t("Horizontální navigace")), i18n()$t("pro orientaci uvnitř sekcí. (Zobrazuje se pouze v členěných sekcích.)")),
          
          tags$li(tags$b(i18n()$t("Zadávací panel")), i18n()$t("pro přidávání jednotlivých položek výkazu.")),
          
          tags$li(tags$b(i18n()$t("Náhled sekce")), i18n()$t("pro zobrazení vykázaných položek vstupů. Vybrané položky zde lze smazat."))
        ),
        
        h2(i18n()$t("Technická pomoc")),
        
        p(i18n()$t("Pokud potřebujete nápovědu nebo máte připomínku, zanechte prosím zprávu v kanálu"), " ", tags$b("#technicka_podpora"), i18n()$t("zde"), a("https://filosoficky.slack.com", href = "https://filosoficky.slack.com/",  target="_blank"), "."),
        
        p(""),p(""),p(""),
        
        p(paste(i18n()$t("Verze:"), as.character(packageVersion("IPCASreporter")))
)
        
          
        )
        
))
    
    })
    
  }
  
  )}
    

