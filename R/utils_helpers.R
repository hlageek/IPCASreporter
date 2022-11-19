

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

                       processed_item <- input[[items]]
                       
                       if ( length(processed_item) > 1 ) {
                           processed_item <- as.character(processed_item)
                           
                           if (processed_item[1] == processed_item[2]) {
                               processed_item <- processed_item[1]
                           }
                       }
                       
                       if (is.character(processed_item)) {
                       processed_item <- utf8::utf8_encode(processed_item)
                       }
                       unlist(paste(
                           processed_item, 
                           collapse = "/"))
                       
                   }
    )

}

# prepare new entry  ---------------------------------------

prep_new_entry <- function(items, all_items, tbl, person_id, year, col_prefix = NULL) {
    
    if (is.null(col_prefix)) {
        col_prefix <- tbl
    }
    entry_df <- tibble::tibble(key = items,
                   value = all_items) %>% 
        tidyr::pivot_wider(tidyselect::everything(),
                           names_from = "key",
                           values_from = "value")
    
    entry_df[[paste0("person_id_", tbl)]] <- person_id
    entry_df[[paste0(col_prefix, "_id_year")]] <- year

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
 
    id_year <- ipcas_db %>%
        dplyr::tbl(tbl) %>% 
        dplyr::select(tidyselect::ends_with("id_year")) %>% 
        colnames()
        
    year <- as.integer( format(Sys.Date(), "%Y"))
    
    pre_filter <- ipcas_db %>%
      dplyr::tbl(tbl) %>%
      dplyr::filter(.data[[paste0("person_id_", tbl)]] %in% person_id) %>% 
      dplyr::filter(.data[[id_year]] == year) 
      
  
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

# sanitize output
sanitize_output <- function(x) {
    
    purrr::map_chr(x, .f = function(x) {
        stringr::str_replace_all(x, "\\\\n|\\\\t", "<br>")
    })
}

# check inputs --------------------------------------

check_inputs <- function(input, list, text = "Zadejte", exclude = NULL) {
    
    if (!is.null(exclude)) {
        list <- list[!grepl(exclude, names(list))]
    }

    purrr::walk2(names(list), list, .f = function(x, y) {
               if (!isTruthy(input[[x]])) showNotification(paste(text, y))
    })
    
   purrr::walk(names(list), ~req(input[[.x]]))
    
    
}

# query for data extracted from API ####
get_asep_sourced_data <- function(ipcas_db, tbl, person_id, year, col_target) {
    
    
    id_year <- ipcas_db %>%
        dplyr::tbl(tbl) %>% 
        dplyr::select(tidyselect::ends_with("id_year")) %>% 
        colnames()
    
    ipcas_db %>% 
        dplyr::tbl(tbl) %>% 
        dplyr::filter(.data[[paste0("person_id_", tbl)]] == person_id) %>% 
        dplyr::filter(.data[[id_year]] == year) %>% 
        dplyr::pull(col_target)
    
}

# render personal preview

render_preview <- function(output,
                           identification,
                           section_i,
                           section_ii,
                           section_iii_undergrad,
                           section_iii_postgrad,
                           section_iii_conference,
                           section_iii_lecture,
                           section_iv,
                           section_v,
                           section_vi_popular,
                           section_vi_school,
                           section_vi_media,
                           section_vii,
                           section_viii_int_projects,
                           section_viii_int_bilateral,
                           section_ix_award,
                           section_ix_review,
                           section_ix_member,
                           section_ix_editions,
                           section_x,
                           section_xi) {
    
    
    
    # Identification  ####
    output$employee_name <- renderText({identification$employee_name})
    output$department <- renderText({identification$department})
    output$fte <- renderText({identification$fte})
    output$email <- renderText({identification$email})
    output$comment <- renderText({identification$comment})
    
    
    # Section I  ####
    
    
    output$section_i <- renderText({
        paste(section_i$publist, collapse = "<br>")
    })
    
    # Section II  ####
    
    output$section_ii <- renderText({
        paste(section_ii$eventlist, collapse = "<br>")
    })
    
    
    # Section III  ####
    
    ## Undergrad  ####
    
    
    output$section_iii_undergrad <- renderText({
        paste(section_iii_undergrad$data)
    })
    
    ## Postgrad  ####
    
    output$section_iii_postgrad <- renderText({
        paste(section_iii_postgrad$data)
    })
    
    ## Conference  ####
    
    output$section_iii_conference_foreign <-  renderText({
        paste(section_iii_conference$foreign)
    })
    output$section_iii_conference_domestic <-  renderText({
        paste(section_iii_conference$domestic)
    })
    
    ## Lecture  ####
    
    output$section_iii_lecture_foreign <-  renderText({
        paste(section_iii_lecture$foreign)
    })
    output$section_iii_lecture_domestic <-  renderText({
        paste(section_iii_lecture$domestic)
    })
    
    
    # Section IV  ####
    
    output$section_iv_funded <-  renderText({
        paste(section_iv$funded)
    })
    output$section_iv_unfunded <-  renderText({
        paste(section_iv$unfunded)
    })
    
    # Section V ####
    
    output$section_v <-  renderText({
        paste(section_v$av21)
    })
    
    # Section VI  ####
    
    ## Events  ####
    
    output$section_vi_popular_events <-  renderText({
        paste(section_vi_popular$events)
    })
    
    ## School events  ####
    
    output$section_vi_school_events <-  renderText({
        paste(section_vi_school$events)
    })
    
    ## Media  ####
    
    output$section_vi_media <-  renderText({
        paste(section_vi_media$media)
    })
    
    # Section VII  ####
    
    output$section_vii <-  renderText({
        paste(section_vii$public)
    })
    
    # Section VIII  ####
    
    output$section_viii_int_projects <-  renderText({
        paste(section_viii_int_projects$projects)
    })
    
    output$section_viii_int_bilateral <-  renderText({
        paste(section_viii_int_bilateral$bilateral)
    })
    
    # Section IX  ####
    
    output$section_ix_award <-  renderText({
        paste(section_ix_award$award)
    })
    
    output$section_ix_review <-  renderText({
        paste(section_ix_review$review)
    })    
    
    output$section_ix_member_domestic <-  renderText({
        paste(section_ix_member$domestic)
    })    
    
    output$section_ix_member_foreign <-  renderText({
        paste(section_ix_member$foreign)
    })        
    
    output$section_ix_editions <-  renderText({
        paste(section_ix_editions$editions)
    })       
    
    # Section X  ####
    
    output$section_x <-  renderText({
        paste(section_x$wip)
    })
    
    # Section XI  ####
    
    output$section_xi <-  renderText({
        paste(section_xi$various)
    })
}