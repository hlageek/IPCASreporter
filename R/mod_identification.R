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

  fluidRow(column(width = 4,
    
    uiOutput(ns("identification_ui")),
    
    actionButton(ns("add"),
                 label = i18n$t("Aktualizovat výkaz"),
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
         "E-mail:",
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
mod_identification_server <- function(id, usr, i18n_r) {
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)


    identification <- reactiveValues()
    
    output$identification_ui <- renderUI({
        
        tagList(shiny.i18n::usei18n(i18n_r()),
            
            textInput(ns("employee_name_first"), 
                      i18n_r()$t("Jméno"), 
                      value = ipcas_db |> 
                          dplyr::tbl("persons") |> 
                          dplyr::filter(person_id == !!usr$person_id) |> 
                          dplyr::pull(name_first),
                      placeholder = "Eva"
            ),
            
            textInput(ns("employee_name_last"), 
                      i18n_r()$t("Příjmení"), 
                      value = ipcas_db |> 
                          dplyr::tbl("persons") |> 
                          dplyr::filter(person_id == !!usr$person_id) |> 
                          dplyr::pull(name_last),
                      placeholder = "Zažímalová"
            ),
            
            textInput(ns("email"),
                      label = i18n_r()$t("E-mailová adresa"), 
                      value = ipcas_db |> 
                          dplyr::tbl("persons") |> 
                          dplyr::filter(person_id == !!usr$person_id) |> 
                          dplyr::pull(email), 
                      placeholder = "@flu.cas.cz"
            ),
            
            selectInput(ns("department"),
                        label = i18n_r()$t("Oddělení"), 
                        selected = ipcas_db |> 
                            dplyr::tbl("departments") |> 
                            dplyr::filter(
                                person_id_departments == !!usr$person_id) |>
                            dplyr::pull(department),
                        choices = c("", departments$department_name),
                        multiple = FALSE
            ),
            
            sliderInput(ns("fte"), 
                        label = i18n_r()$t("Úvazek"), 
                        value = ifelse(isTruthy(ipcas_db |> 
                            dplyr::tbl("persons") |> 
                            dplyr::filter(person_id == !!usr$person_id) |> 
                            dplyr::pull(fte)),
                            ipcas_db |> 
                                dplyr::tbl("persons") |> 
                                dplyr::filter(person_id == !!usr$person_id) |> 
                                dplyr::pull(fte),
                            0), 
                        min = 0, 
                        max = 1, 
                        step = 0.01
            ),
            
            textAreaInput(ns("comment"), 
                          label = i18n_r()$t("Poznámka"),
                          value = ipcas_db |> 
                              dplyr::tbl("persons") |> 
                              dplyr::filter(person_id == !!usr$person_id) |> 
                              dplyr::pull(comment),
                          placeholder = i18n_r()$t("Např. změny ve výši úvazku v průběhu roku.")
            )
        )
        
    })
    
    observeEvent(usr$person_id, {

        persons <- ipcas_db |> 
            dplyr::tbl("persons") |> 
            dplyr::filter(person_id == !!usr$person_id) |> 
            dplyr::collect()

        department <- ipcas_db |> 
            dplyr::tbl("departments") |> 
            dplyr::filter(person_id_departments == !!usr$person_id) |> 
            dplyr::pull("department")
        
        identification$employee_name <- paste(persons$name_first, persons$name_last)
        identification$department <- department
        identification$fte <- persons$fte
        identification$comment <- persons$comment
        identification$email <- persons$email
        
        output$employee_name <- renderText({paste(persons$name_first, 
                                                  persons$name_last)
            })
        output$department <- renderText({department})
        output$fte <- renderText({persons$fte})
        output$email <- renderText({persons$email})
        output$comment <- renderText({persons$comment})
        
    })

    observeEvent(input$add, {

         #browser()
        
        persons <- ipcas_db |> 
            dplyr::tbl("pubs") |> 
            dplyr::filter(person_id_pubs == !!usr$person_id) |> 
            dplyr::collect()
        
        pool::dbExecute(ipcas_db, 
                        paste0( "INSERT INTO persons",
                                " (",
                                "person_id,", 
                                "name_first,", 
                                "name_last,", 
                                "email,", 
                                "fte,", 
                                "comment",
                                ")",
                                " VALUES(",
                                "'", usr$person_id, "',",
                                "'", input$employee_name_first, "',",
                                "'", input$employee_name_last, "',",
                                "'", input$email, "',",
                                "'", input$fte, "',",
                                "'", input$comment, "'",
                                ")",
                                " ON DUPLICATE KEY UPDATE", 
                                " name_first = '", input$employee_name_first, "',",
                                " name_last = '",  input$employee_name_last, "',", 
                                " email = '",  input$email, "',",
                                " fte = '",  input$fte, "',",
                                " comment = '", input$comment, "'"
                                )
                        )
        
        pool::dbExecute(ipcas_db, 
                        paste0( "INSERT INTO departments",
                                " (person_id_departments, department)",
                                " VALUES(",
                                "'", usr$person_id, "',",
                                "'", input$department, "'",
                                ")",
                                " ON DUPLICATE KEY UPDATE", 
                                " person_id_departments = '", usr$person_id, "',",
                                " department = '", input$department, "'"
                        )
        )
        
        persons <- ipcas_db |> 
            dplyr::tbl("persons") |> 
            dplyr::filter(person_id == !!usr$person_id) |> 
            dplyr::collect()
        
        department <- ipcas_db |> 
            dplyr::tbl("departments") |> 
            dplyr::filter(person_id_departments == !!usr$person_id) |> 
            dplyr::pull("department")
        
        identification$employee_name <- paste(persons$name_first, persons$name_last)
        identification$department <- department
        identification$fte <- persons$fte
        identification$comment <- persons$comment
        identification$email <- persons$email
        
        output$employee_name <- renderText({paste(persons$name_first, 
                                                  persons$name_last)
            })
        output$department <- renderText({department})
        output$fte <- renderText({persons$fte})
        output$email <- renderText({persons$email})
        output$comment <- renderText({persons$comment})
        
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

    


