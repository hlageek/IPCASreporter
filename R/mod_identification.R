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
    persons <- reactiveValues()
    yearly <- reactiveValues()
    department <- reactiveValues()
    
    observeEvent(usr$person_id,{
        
       person_exists <- ipcas_db %>% 
            dplyr::tbl("persons") %>% 
            dplyr::filter(person_id == !!usr$person_id) %>% 
            dplyr::pull(person_id)
        
       if (length(person_exists) == 0) {
           
           new_person_df <- tibble::tibble(
               person_id = as.integer(usr$person_id),
               name_first = usr$name_first,
               name_last = usr$name_last,
               email = usr$user
           )
           
           DBI::dbAppendTable(ipcas_db, "persons", new_person_df)
           
       }
    
       persons$data <- ipcas_db %>%
           dplyr::tbl("persons") %>%
           dplyr::filter(person_id == !!usr$person_id) %>%
           dplyr::collect()
       
       yearly$data <- ipcas_db %>%
           dplyr::tbl("yearly") %>%
           dplyr::filter(person_id_yearly == !!usr$person_id) %>%
           dplyr::collect()
       
       department$data <- ipcas_db %>%
           dplyr::tbl("departments") %>%
           dplyr::filter(person_id_departments == !!usr$person_id) %>%
           dplyr::pull("department")

    output$identification_ui <- renderUI({

        tagList(

            textInput(ns("employee_name_first"),
                      i18n()$t("Jméno"),
                      value = persons$data$name_first,
                      placeholder = "Eva"
            ),

            textInput(ns("employee_name_last"),
                      i18n()$t("Příjmení"),
                      value = persons$data$name_last,
                      placeholder = "Zažímalová"
            ),

            textInput(ns("email"),
                      label = i18n()$t("E-mailová adresa"),
                      value = persons$data$email,
                      placeholder = "@flu.cas.cz"
            ),

            selectInput(ns("department"),
                        label = i18n()$t("Oddělení"),
                        selected = department$data,
                        choices = c("", IPCASreporter::departments$department_name),
                        multiple = FALSE
            ),

            sliderInput(ns("fte"),
                        label = i18n()$t("Úvazek"),
                        value = ifelse(isTruthy(yearly$data$fte),
                                       yearly$data$fte,
                            0),
                        min = 0,
                        max = 1,
                        step = 0.01
            ),

            textAreaInput(ns("comment"),
                          label = i18n()$t("Poznámka"),
                          value = yearly$data$comment,
                          placeholder = i18n()$t("Např. změny ve výši úvazku v průběhu roku.")
            )
        )

    })




        identification$employee_name <- paste(persons$data$name_first, persons$data$name_last)
        identification$department <- department$data
        identification$fte <- yearly$data$fte
        identification$comment <- yearly$data$comment
        identification$email <- persons$data$email

        output$employee_name <- renderText({paste(identification$employee_name)
            })
        output$department <- renderText({identification$department})
        output$fte <- renderText({identification$fte})
        output$email <- renderText({identification$email})
        output$comment <- renderText({identification$comment})

    })

    observeEvent(input$add, {

        checks <- c("employee_name_first",
                    "employee_name_last",
                    "email",
                    "department")
        checks <- stats::setNames(i18n()$t(c(
            "Jméno:",
            "Příjmení",
            "E-mail:",
            "Oddělení:"
        )),
        checks)
        check_inputs(input = input,
                     list = checks,
                     text = i18n()$t("Zadejte"))

       
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
        
        
        persons$data <- ipcas_db %>%
            dplyr::tbl("persons") %>%
            dplyr::filter(person_id == !!usr$person_id) %>%
            dplyr::collect()
        
        yearly$data <- ipcas_db %>%
            dplyr::tbl("yearly") %>%
            dplyr::filter(person_id_yearly == !!usr$person_id) %>%
            dplyr::collect()
        
        department$data <- ipcas_db %>%
            dplyr::tbl("departments") %>%
            dplyr::filter(person_id_departments == !!usr$person_id) %>%
            dplyr::pull("department")
        
        identification$employee_name <- paste(persons$data$name_first, persons$data$name_last)
        identification$department <- department$data
        identification$fte <- yearly$data$fte
        identification$comment <- yearly$data$comment
        identification$email <- persons$data$email
        
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
