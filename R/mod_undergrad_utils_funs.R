radioButtons_helper <- function(ns, label, id) {
  tagList(
    
    tags$div(
      # tags$style(
      #   HTML(
      #     "
      #   label{
      #     float:left;
      #   }
      # "
      #   )),
      tags$br(),
     
    
    radioButtons(inputId = ns(paste0("undergrad_type_", id)), 
                 label = paste0(label, ":", stringi::stri_dup(intToUtf8(160), 1)), 
                 choices = c("ano" = "ano", "ne" = "ne"), 
                 selected = "ne",
                 inline = TRUE)
    
    )
  )
}

undergrad_types <- function(id) {
    
    ns <- NS(id)
    
    undergrad_type <-  c("prednasky" = "Přednášky", 
                         "seminare" = "Semináře", 
                         "cviceni" = "Cvičení",  
                         "vedeni" = "Vedení bakalářských a diplomových prací", 
                         "texty" = "Učební texty")

    purrr::map2(undergrad_type, 
                names(undergrad_type), 
                ~radioButtons_helper(ns, label = .x, id = .y)
    )    
    
    
}
