radioButtons_helper <- function(ns, label, id) {
  tagList(
    
    tags$div(
      tags$style(
        HTML(
          "
        label{
          float:left;
        }
      "
        )),
      tags$br(),
     
    
    radioButtons(inputId = ns(paste0("type_", id)), 
                 label = paste0(label, ":", stringi::stri_dup(intToUtf8(160), 1)), 
                 choices = c("ano", "ne"), 
                 selected = "ne",
                 inline = TRUE)
    
    )
  )
}

undergrad_types <- function(id) {
    
    ns <- NS(id)
    
    undergrad_type <-  c("Přednášky", 
                         "Semináře", 
                         "Cvičení",  
                         "Vedení bakalářských a diplomových prací", 
                         "Učební texty")

    purrr::map2(undergrad_type, 
                seq_along(undergrad_type), 
                ~radioButtons_helper(ns, label = .x, id = .y)
    )    
    
    
}
