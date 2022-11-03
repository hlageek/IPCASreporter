#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
        email_password = NULL,
        email_default = NULL,
        dbname = NULL,
        dbusername = NULL,
        dbpassword = NULL) {
  with_golem_options(
    app = shinyApp(
      ui = shinymanager::secure_app(app_ui),
      server = app_server,
      onStart = purrr::partial(eval, expr = make_pool, envir = globalenv())
      ),
    golem_opts = list(translator = shiny.i18n::Translator$new(translation_csvs_path = app_sys("app/www/translations")),
        email_password = email_password,
        email_default = email_default,
        dbname = dbname,
        dbusername = dbusername,
        dbpassword = dbpassword
        )
  )
}
