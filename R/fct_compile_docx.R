

compile_docx <- function(identification, 
                         section_i,
                         section_iii_undergrad,
                         section_iii_postgrad,
                         section_iii_conference,
                         section_iii_lecture,
                         section_iv) {
    
    
    doc <- officer::read_docx(here::here("inst", "app", "www", "annual_report_ipcas.docx")) %>%
        officer::body_replace_text_at_bkm("employee_name", identification$employee_name) %>%
        officer::body_replace_text_at_bkm("department",identification$department) %>%
        officer::body_replace_text_at_bkm("fte", as.character(identification$fte)) %>%
        add_doc_section("comment",
                        identification$comment) %>% 
        officer::cursor_bookmark("pubs") %>% 
        officer::body_add_par("") %>% 
        body_add_par_nf(format_html_citation(section_i$publist)) %>%
        officer::body_replace_text_at_bkm("pubs", "") %>% 
        add_doc_section("undergrad",
                        section_iii_undergrad$data) %>% 
        add_doc_section("postgrad",
                        section_iii_postgrad$data) %>% 
        add_doc_section("conference_foreign",
                        section_iii_conference$foreign) %>% 
        add_doc_section("conference_domestic",
                        section_iii_conference$domestic) %>% 
        add_doc_section("lecture_foreign",
                        section_iii_lecture$foreign) %>% 
        add_doc_section("lecture_domestic",
                        section_iii_lecture$domestic) %>% 
        add_doc_section("funded",
                        section_iv$funded) %>% 
        add_doc_section("unfunded",
                        section_iv$unfunded)
}
