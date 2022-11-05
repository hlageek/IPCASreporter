

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
    
    pattern <- paste0(" Druh: ", type, "{0,2}\\.$")
        
    vec %>% 
        stringr::str_replace_all(" {2,}", "") %>% 
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

collect_items <- function(items, input) {

    purrr::map_chr(items, 
                   .f = function(items) {
                       
                       unlist(paste(input[[items]], collapse = "/"))
                       
                   }
    )

}

# prepare new entry  ---------------------------------------

prep_new_entry <- function(items, all_items, tbl, person_id, year) {
    
    entry_df <- tibble::tibble(key = items,
                   value = all_items) %>% 
        tidyr::pivot_wider(tidyselect::everything(),
                           names_from = "key",
                           values_from = "value")
    
    entry_df[[paste0("person_id_", tbl)]] <- person_id
    entry_df[[paste0(tbl, "_id_year")]] <- year

    entry_df
    
}

# format input that includes a date ---------------------------------------

format_input <- function(x) { 
    
    if (!is.na(as.Date(x[1], optional = TRUE))) {
        
        x <- format(as.Date(x), "%d. %m. %Y")
        
    }
    paste(as.character(unique(x)), collapse = " - ")
    
}


# transform table ---------------------------------------


transform_table <- function(ipcas_db, tbl, person_id, tbl_id, filter_col = NULL, filter_val = NULL, names_df) {
  
  pre_filter <- ipcas_db %>%
      dplyr::tbl(tbl) %>%
      dplyr::filter(.data[[paste0("person_id_", tbl)]] == person_id)
  
  if (!is.null(filter_col)) {
      post_filter <- pre_filter %>% 
          dplyr::filter(.data[[filter_col]] == filter_val)
          
  } else {
      post_filter <- pre_filter
  }
  
  post_filter %>% 
      dplyr::select(-.data[[paste0("person_id_", tbl)]]) %>%
      tidyr::pivot_longer(-.data[[tbl_id]],
                          names_to = "key",
                          values_to = "value") %>%
      dplyr::collect() %>%
      dplyr::left_join(names_df, by = "key") %>%
      dplyr::filter(!is.na(names)) %>% 
      dplyr::filter(value != "") %>% 
      tidyr::unite("value", c(names, value), sep = " ") %>%
      dplyr::select(-key) %>%
      dplyr::group_by(.data[[tbl_id]]) %>%
      dplyr::summarise(data = stringr::str_flatten(value, collapse = "<br>"))

}

# check inputs --------------------------------------

check_inputs <- function(input, list, text = "Zadejte") {
    
    purrr::walk2(names(list), list, .f = function(x, y) {
               if (!isTruthy(input[[x]])) showNotification(paste(text, y))
    })
    
   purrr::walk(names(list), ~req(input[[.x]]))
    
    
}