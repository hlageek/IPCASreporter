

compile_docx <- function(identification, 
                         section_i,
                         section_iii_undergrad,
                         section_iii_postgrad,
                         section_iii_conference,
                         section_iii_lecture,
                         section_iv,
                         section_v,
                         section_vi_popular,
                         section_vi_school,
                         section_vii,
                         section_viii_int_projects,
                         section_viii_int_bilateral,
                         section_ix_award,
                         section_ix_review,
                         section_ix_member,
                         section_x,
                         section_xi
                         ) {
    
    
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
                        section_iv$unfunded) %>% 
        add_doc_section("av21",
                        section_v$av21) %>% 
        add_doc_section("events",
                        section_vi_popular$events) %>% 
        add_doc_section("school",
                        section_vi_school$events) %>% 
        add_doc_section("public",
                        section_vii$public) %>% 
        add_doc_section("int_projects",
                        section_viii_int_projects$projects) %>% 
        add_doc_section("int_bilateral",
                        section_viii_int_bilateral$bilateral) %>% 
        add_doc_section("section_ix_award",
                        section_ix_award$award) %>% 
        add_doc_section("section_ix_review",
                        section_ix_review$review) %>% 
        add_doc_section("section_ix_member_domestic",
                        section_ix_member$domestic) %>% 
        add_doc_section("section_ix_member_foreign",
                        section_ix_member$foreign) %>% 
        add_doc_section("section_x",
                        section_x$wip) %>% 
        add_doc_section("section_xi",
                        section_xi$data)
        
        
        
}
