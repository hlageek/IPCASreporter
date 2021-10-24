

#--------------------------------------
# word formatting utils

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

#--------------------------------------
# word formatting utils

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

#--------------------------------------
# add paragraphs

body_add_par_n <- function(doc, value) {
    i <- 1
    n <- length(value)
    while (i<=n) {
        
        doc <- officer::body_add_fpar(doc, 
                                      officer::fpar(value[[i]], 
                                                    fp_t = officer:: fp_text(font.size = 12,
                                                                             font.family = "Times New Roman"))) 
        i <- i+1
    }
    
    doc
}

# formatted
body_add_par_nf <- function(doc, value) {
    i <- 1
    n <- length(value)
    while (i<=n) {
        
        doc <- officer::body_add_fpar(doc, value[[i]])
        i <- i+1
    }
    
    doc
}
#--------------------------------------


#--------------------------------------
# collapse <br> separated reactive values for Word

collapse_br <- function(list_value) {
    if (isTruthy(list_value)) {
purrr::reduce(list_value, paste) %>%  
    strsplit("<br>") %>% 
    unlist() %>% 
        trimws()
    } else {""}
}

# filter publication by type  ---------------------------------------

filter_pub_type <- function(vec, type) {
    
    pattern <- paste0(" Druh: ", type, ".{0,2}\\.$")
        
    vec %>% 
        stringr::str_subset(pattern) %>% 
        stringr::str_replace(pattern, "")
        
}

# add doc section  ---------------------------------------

add_doc_section <- function(doc, bookmark, list_data) {
    
    doc %>% 
    officer::cursor_bookmark(bookmark) %>%
    officer::body_add_par("") %>%
    body_add_par_n(collapse_br(list_data)) %>%
    officer::body_replace_text_at_bkm(bookmark, "")
}

# add doc section formatted ---------------------------------------

add_doc_f_section <- function(doc, bookmark, f_data) {
    doc %>%
    officer::cursor_bookmark(bookmark) %>% 
    officer::body_add_par("") %>% 
    body_add_par_nf(format_html_citation(f_data)) %>%
    officer::body_replace_text_at_bkm(bookmark, "") 
}

# item collection from multi-input ---------------------------------------

collect_items <- function(items, item_names, item_values) {

        all_items <- list()
    
    
for (i in seq_along(items)) {
    
    all_items <- c(all_items, paste(item_names[i], item_values[i]))
    
}

all_items

}