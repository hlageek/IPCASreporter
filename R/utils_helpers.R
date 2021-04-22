get_asep <- function(asep_code) {
    # httr::GET(
    #     paste0("https://asep.lib.cas.cz/arl-cav/cs/vysledky/?st=feed&feed=rss&sort=DEFAULT&field=T001N&boolop1=and&term=", asep_code)
    #     ) %>% 
    #     httr::content() %>% 
    #     xml2::xml_find_first("//item//title") %>% 
    #     xml2::xml_text() 
    
 asep_citation <- "NODATA"
 
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
