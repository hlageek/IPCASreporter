radioButtons_helper_postgrad <- function(ns, label, id) {
    tagList(
        
        tags$div(
      #       tags$style(
      #           HTML(
      #               "
      #   label{
      #     float:left;
      #   }
      # "
      #           )),
      tags$br(),
      
      
      radioButtons(inputId = ns(paste0("postgrad_type_", id)), 
                   label = paste0(label, ":", stringi::stri_dup(intToUtf8(160), 1)), 
                   choices = c("ano", "ne"), 
                   selected = "ne",
                   inline = TRUE)
      
        )
    )
}

postgrad_types <- function(id) {
    
    ns <- NS(id)
    
    postgrad_type <-  c("prednasky" = "Přednášky", 
                         "seminare" = "Semináře", 
                         "cviceni" = "Cvičení",  
                         "vedeni" = "Vedení dizertačních prací", 
                         "texty" = "Učební texty")
    
    purrr::map2(postgrad_type, 
                names(postgrad_type), 
                ~radioButtons_helper_postgrad(ns, label = .x, id = .y)
    )    
    
    
}
