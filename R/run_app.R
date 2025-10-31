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
  dbpassword = NULL,
  credentials_path = NULL,
  credentials_pass = NULL,
  department_heads = NULL,
  deadline = NULL,
  exception = NULL
) {
  with_golem_options(
    app = shinyApp(
      ui = shinymanager::secure_app(
        app_ui,
        tags_top = tags$img(
          src = "https://pbs.twimg.com/profile_images/1257390834074386432/wpu1564y_400x400.jpg",
          height = "30%",
          style = "margin-right: 20px"
        ),
        tags_bottom = tags$div(
          style = "text-align: right; font-size: small;",
          tags$a(
            href = "https://app.flu.cas.cz/vykaz_obnovitpristup/",
            target = "_blank",
            "Password recovery"
          )
        ),
        enable_recover = TRUE,
        enable_admin = TRUE,
        fab_position = "bottom-left"
      ),
      server = app_server,
      onStart = purrr::partial(eval, expr = make_globals, envir = globalenv())
    ),

    golem_opts = list(
      translator = shiny.i18n::Translator$new(
        translation_csvs_path = app_sys("app/www/translations")
      ),
      email_password = email_password,
      email_default = email_default,
      dbname = dbname,
      dbusername = dbusername,
      dbpassword = dbpassword,
      credentials_path = credentials_path,
      credentials_pass = credentials_pass,
      department_heads = department_heads,
      deadline = deadline,
      exception = exception
    )
  )
}
