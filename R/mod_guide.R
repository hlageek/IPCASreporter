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
      label = i18n$t("Instructions"),
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
        modalButton(i18n()$t("Close"))),
      easyClose = TRUE,

      tagList(

        h2(i18n()$t("O aplikaci")),

        p(i18n()$t("Pracovní výkaz slouží mj. ke sběru dat k výroční zprávě FLÚ, dále jako podklad pro návrhy v oblasti odměnovaní zaměstnanců, konkrétně pro stanovení odměn za mimořádný pracovní výkon v daném kalendářním roce a určení výše osobního příplatku pro nový kalendářní rok, event. k atualizaci údajů na webové stránce oddělení."), tags$b(i18n()$t("Do výkazu zanášejte jen ty výsledky, které nevykazujete na jiném pracovišti."))),

        h2(i18n()$t("Instructions")),

        tags$li(i18n()$t("Your name will be used to search the ASEP repository to provide selectable options in sections"), tags$b( "I. & II.")),

        tags$li(i18n()$t("Use the menu on the left to navigate in the application and continue to fill all relevant"), tags$b(i18n()$t("sekce výkazu."))),

        tags$li(i18n()$t("Use the"), tags$b(i18n()$t("Save")), i18n()$t("button to generate a link that will restore your work on the report if you need to come back to it later.")),

        tags$li(i18n()$t("Use the"), tags$b(i18n()$t("Download")), i18n()$t("button to generate a MS Word version of the report.")),

        #tags$li(i18n()$t("Use the"), tags$b(i18n()$t("Submit")), i18n()$t("button to submit the report. You will receive a confirmation email after the submission.")),

        #tags$li(HTML("<i class='fa fa-warning'></i>"), i18n()$t("To avoid data loss, do not refresh the browser while using the app!"), style = "color:red"),

        h3(i18n()$t("Navigation")),

        img(src="https://owncloud.cesnet.cz/index.php/s/sEf5XIUg7d0jNKq/download",
            width="100%"),

        tags$ol(
          tags$li(tags$b(i18n()$t("Vertical navigation")), i18n()$t("for the application and switching between"), tags$b(i18n()$t("Preview")), i18n()$t("and"), tags$b(i18n()$t("sekcemi výkazu")), "."),

          tags$li(tags$b(i18n()$t("Horizontal navigation")), i18n()$t("for the inside of the report sections. (Only appears in subdivided sections.)")),

          tags$li(tags$b(i18n()$t("Section input")), i18n()$t("panel for adding individual report items.")),

          tags$li(tags$b(i18n()$t("Section preview")), i18n()$t("to display previous inputs. Selected inputs can be deleted here."))
        ),

        h2(i18n()$t("Help")),

        p(i18n()$t("If you need help or have a comment, please leave a message in the "), tags$b("#technicka_podpora"), i18n()$t("channel at"), a("https://filosoficky.slack.com", href = "https://filosoficky.slack.com/",  target="_blank"), "."),

        p(""),p(""),p(""),

        p(paste(i18n()$t("Version:"), as.character(packageVersion("IPCASreporter")))
)


        )

))

    })

  }

  )}
