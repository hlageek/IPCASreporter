#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = shinymanager::secure_app(app_ui),
      server = app_server,
      enableBookmarking = "server"
    ),
    golem_opts = list(translator = shiny.i18n::Translator$new(translation_csvs_path = "inst/app/www/"),
        email_password = email_password,
        email_default = email_default
        )
  )
}
