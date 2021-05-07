get_asep <- function(asep_code) {
    
    query <- paste0("@attr 98=2 @attr 1=2426 '", asep_code, "'")
    
    asep_citation <- httr::GET(url = "https://asep.lib.cas.cz/i2/i2.ws.cls",
                               query = list(method = "search", 
                                            db = "CavUnEpca", 
                                            query = query,
                                            fmt = "xml")) %>% 
        httr::content(as = "parsed", "text/html", "utf-8")
    
    asep_citation <- asep_citation %>% rvest::html_node("body") %>% 
        rvest::html_nodes(xpath = '//*[@tag="Tbc"]') %>% 
        rvest::html_text2()
    
    if (length(asep_citation) > 0) {
 
        asep_citation
        
    } else {
        "No data found for the ASEP item code."
    }
    
}




############################
# word formatting util

format_html_citation <- function(html_citation) {

    citation <- sapply(html_citation, strsplit, "</?em>")
    fp_normal <- officer::fp_text()
    fp_italic <- update(fp_normal, italic = TRUE)
    
    
    process_format <- function(citation) {
        officer::fpar(
        officer::ftext(citation[1], fp_normal), 
        officer::ftext(citation[2], fp_italic),
        officer::ftext(citation[3], fp_normal) 

        )

    }
    
   processed <-  purrr::map(citation, process_format) 

   value <- do.call(officer::block_list, processed)
   
   return(value)
}


add_blocks <- function( x, blocks, pos = "after" ){
    

    if( length(blocks) > 0 ){
        pos_vector <- rep("after", length(blocks))
        for(i in seq_along(blocks) ){
            
            wml <- officer::to_wml(blocks[[i]])
            x <- officer::cursor_end(x)
            x <- officer::body_add_xml(x, wml, pos = "after")
            

        }
    }
    
    x
}

body_add_par_n <- function(doc, value) {
    i <- 1
    n <- length(value)
    while (i<=n) {
        
        doc <- officer::body_add_fpar(doc, value[[i]])
        i <- i+1
    }
    
    doc
}
