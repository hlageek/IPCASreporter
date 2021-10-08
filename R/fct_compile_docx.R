

compile_docx <- function(identification, 
                         section_i,
                         section_iii_undergrad,
                         section_iii_postgrad) {
    
    #browser()
    doc <- officer::read_docx(here::here("inst", "app", "www", "annual_report_ipcas.docx")) %>%
        officer::body_replace_text_at_bkm("employee_name", identification$employee_name) %>%
        officer::body_replace_text_at_bkm("department",identification$department) %>%
        officer::body_replace_text_at_bkm("fte", as.character(identification$fte)) %>%
        officer::cursor_bookmark("pubs") %>% 
        officer::body_add_par("") %>% 
        body_add_par_nf(format_html_citation(section_i$publist)) %>%
        officer::body_replace_text_at_bkm("pubs", "") %>% 
        officer::cursor_bookmark("undergrad") %>% 
        officer::body_add_par("") %>% 
        body_add_par_n(collapse_br(section_iii_undergrad)) %>% 
        officer::body_replace_text_at_bkm("undergrad", "") %>% 
        officer::cursor_bookmark("postgrad") %>% 
        officer::body_add_par("") %>% 
        body_add_par_n(collapse_br(section_iii_postgrad)) %>% 
        officer::body_replace_text_at_bkm("postgrad", "") 
}
