#--------------------------------------
# call asep api based on name
get_asep <- function(author_name) {
    
    query <- paste0("@attr 98=2 @and @and @attr 1=1 '", author_name, "' @attr 1=2462 'FLU-F' @attr 1=31 @or '", format(Sys.Date(), "%Y"), "''", format(Sys.Date()-365, "%Y"), "'")
    
    asep_result <- httr::GET(url = "https://asep.lib.cas.cz/i2/i2.ws.cls",
                             query = list(method = "search", 
                                          db = "CavUnEpca", 
                                          query = query,
                                          from = 1,
                                          to = 5,
                                          sort = "1=31 i>",
                                          fmt = "xml")) %>% 
        httr::content(as = "parsed", "text/html", "utf-8")
    
    asep_citation <- asep_result %>% rvest::html_node("body") %>% 
        rvest::html_nodes(xpath = '//*[@tag="Tbc"]') %>% 
        rvest::html_text2() %>% 
        stringr::str_replace_all("\\\\n", ". ")
    
    asep_handles<- asep_result %>% rvest::html_node("body") %>% 
        rvest::html_nodes(xpath = '//*[@tag="C60"]') %>% 
        rvest::html_text2()  
    
    asep_types <- asep_result %>% rvest::html_node("body") %>% 
        rvest::html_nodes(xpath = '//*[@tag="970"]') %>% 
        rvest::html_text2()  
    
    asep_record <- paste(asep_citation, 
                         asep_handles, 
                         "Druh:",
                         asep_types)
    
    if (length(asep_record) > 0) {
        
        asep_record
        
    } else {
       
        NULL
    }
    
}

#--------------------------------------
# clean pubs names for display




