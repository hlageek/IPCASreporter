make_add_remove_ui <- function(ns) {
    
    tagList(
    actionButton(ns("save"), label = "Add to report"),
    
    actionButton(ns("remove"), label = "Remove from report")
    )
}

call_add_ui <- function(object, name, data, input_add = input$save, ns = ns, session = session, input = input) {
    
    observeEvent( input_add , {
        object[[name]] <- c(object[[name]], data())
    })
    
    
}

call_remove_ui <- function(object, name, data, input = input$remove) {
    
    observeEvent( input , {
        
        if (length(object[[name]]) <= 1) {
            
            object[[name]]<- c()
            
        } else {
            
            object[[name]]<- object[[name]][1:(length(object[[name]])-1)]
            
        }
        
        
    })
    
    
}