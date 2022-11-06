#' identification UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_identification_ui <- function(id, i18n){
  ns <- NS(id)

  fluidRow(shiny.i18n::usei18n(i18n),
           column(width = 4,

    uiOutput(ns("identification_ui")),

    actionButton(ns("add"),
                 label = i18n$t("Zadat do výkazu"),
                 icon = icon("check"),
                 class = "btn-success"
                 )


  ),

  column(width = 8,

         br(),
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
         i18n$t("Poznámka:"),
         textOutput(ns("comment"), inline = TRUE)

         )


  )
}

#' identification Server Function
#'
#' @noRd
mod_identification_server <- function(id, usr, i18n) {
  moduleServer(id, function(input, output, session) {

    ns <- NS(id)


    identification <- reactiveValues()

    output$identification_ui <- renderUI({

        tagList(

            textInput(ns("employee_name_first"),
                      i18n()$t("Jméno"),
                      value = ipcas_db |>
                          dplyr::tbl("persons") |>
                          dplyr::filter(person_id == !!usr$person_id) |>
                          dplyr::pull(name_first),
                      placeholder = "Eva"
            ),

            textInput(ns("employee_name_last"),
                      i18n()$t("Příjmení"),
                      value = ipcas_db |>
                          dplyr::tbl("persons") |>
                          dplyr::filter(person_id == !!usr$person_id) |>
                          dplyr::pull(name_last),
                      placeholder = "Zažímalová"
            ),

            textInput(ns("email"),
                      label = i18n()$t("E-mailová adresa"),
                      value = ipcas_db |>
                          dplyr::tbl("persons") |>
                          dplyr::filter(person_id == !!usr$person_id) |>
                          dplyr::pull(email),
                      placeholder = "@flu.cas.cz"
            ),

            selectInput(ns("department"),
                        label = i18n()$t("Oddělení"),
                        selected = ipcas_db |>
                            dplyr::tbl("departments") |>
                            dplyr::filter(
                                person_id_departments == !!usr$person_id) %>%
                            dplyr::pull(department),
                        choices = c("", IPCASreporter::departments$department_name),
                        multiple = FALSE
            ),

            sliderInput(ns("fte"),
                        label = i18n()$t("Úvazek"),
                        value = ifelse(isTruthy(ipcas_db |>
                            dplyr::tbl("yearly") |>
                            dplyr::filter(person_id_yearly == !!usr$person_id) |>
                            dplyr::pull(fte)),
                            ipcas_db %>%
                                dplyr::tbl("yearly") %>%
                                dplyr::filter(person_id_yearly == !!usr$person_id) %>%
                                dplyr::pull(fte),
                            0),
                        min = 0,
                        max = 1,
                        step = 0.01
            ),

            textAreaInput(ns("comment"),
                          label = i18n()$t("Poznámka"),
                          value = ipcas_db |>
                              dplyr::tbl("yearly") |>
                              dplyr::filter(person_id_yearly == !!usr$person_id) |>
                              dplyr::pull(comment),
                          placeholder = i18n()$t("Např. změny ve výši úvazku v průběhu roku.")
            )
        )

    })

    observeEvent(usr$person_id, {

        persons <- ipcas_db %>%
            dplyr::tbl("persons") %>%
            dplyr::filter(person_id == !!usr$person_id) %>%
            dplyr::collect()
        
        yearly <- ipcas_db %>%
            dplyr::tbl("yearly") %>%
            dplyr::filter(person_id_yearly == !!usr$person_id) %>%
            dplyr::collect()

        department <- ipcas_db %>%
            dplyr::tbl("departments") %>%
            dplyr::filter(person_id_departments == !!usr$person_id) %>%
            dplyr::pull("department")

        identification$employee_name <- paste(persons$name_first, persons$name_last)
        identification$department <- department
        identification$fte <- yearly$fte
        identification$comment <- yearly$comment
        identification$email <- persons$email

        output$employee_name <- renderText({paste(identification$employee_name)
            })
        output$department <- renderText({identification$department})
        output$fte <- renderText({identification$fte})
        output$email <- renderText({identification$email})
        output$comment <- renderText({identification$comment})

    })

    observeEvent(input$add, {

       
        pool::dbExecute(ipcas_db,
                        paste0( "INSERT INTO persons",
                                " (",
                                "person_id,",
                                "name_first,",
                                "name_last,",
                                "email",
                                ")",
                                " VALUES(",
                                "'", usr$person_id, "',",
                                "'", input$employee_name_first, "',",
                                "'", input$employee_name_last, "',",
                                "'", input$email, "'",
                                ")",
                                " ON DUPLICATE KEY UPDATE",
                                " name_first = '", input$employee_name_first, "',",
                                " name_last = '",  input$employee_name_last, "',",
                                " email = '",  input$email, "'"
                        )
        )
        
        yearly_id <- ipcas_db %>%
            dplyr::tbl("yearly") %>%
            dplyr::filter(person_id_yearly == !!usr$person_id) %>%
            dplyr::pull("yearly_id")
        pool::dbExecute(ipcas_db, 
                        "DELETE FROM yearly WHERE yearly_id IN (?)",
                        params = list(yearly_id))
        department_id <- ipcas_db %>%
            dplyr::tbl("departments") %>%
            dplyr::filter(person_id_departments == !!usr$person_id) %>%
            dplyr::pull("department_id")
        pool::dbExecute(ipcas_db, 
                        "DELETE FROM departments WHERE department_id IN (?)",
                        params = list(department_id))
        
        
        new_entry_yearly <- tibble::tibble(
            person_id_yearly = usr$person_id,
            yearly_id_year = as.integer( format(Sys.Date(), "%Y")),
            fte = input$fte,
            comment = input$comment
        )
        DBI::dbAppendTable(ipcas_db, "yearly", new_entry_yearly)
        
        new_entry_departments <- tibble::tibble(
            person_id_departments = usr$person_id,
            department_id_year = as.integer( format(Sys.Date(), "%Y")),
            department = input$department
        )
        DBI::dbAppendTable(ipcas_db, "departments", new_entry_departments)
        
        
        persons <- ipcas_db %>%
            dplyr::tbl("persons") %>%
            dplyr::filter(person_id == !!usr$person_id) %>%
            dplyr::collect()
        
        yearly <- ipcas_db %>%
            dplyr::tbl("yearly") %>%
            dplyr::filter(person_id_yearly == !!usr$person_id) %>%
            dplyr::collect()
        
        department <- ipcas_db %>%
            dplyr::tbl("departments") %>%
            dplyr::filter(person_id_departments == !!usr$person_id) %>%
            dplyr::pull("department")
        
        identification$employee_name <- paste(persons$name_first, persons$name_last)
        identification$department <- department
        identification$fte <- yearly$fte
        identification$comment <- yearly$comment
        identification$email <- persons$email
        
        output$employee_name <- renderText({paste(identification$employee_name)
        })
        output$department <- renderText({identification$department})
        output$fte <- renderText({identification$fte})
        output$email <- renderText({identification$email})
        output$comment <- renderText({identification$comment})

            })
    return(identification)
  })
}



identificationApp <- function() {
  ui <- fluidPage(
    mod_identification_ui("identification_ui_1")
  )
  server <- function(input, output, session) {

    mod_identification_server("identification_ui_1")
  }
  shinyApp(ui, server)
}
