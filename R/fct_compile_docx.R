

compile_docx <- function(identification, 
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
                         section_xi
                         ) {
    
    # browser()
    doc <- officer::read_docx(system.file("app/www/annual_report_ipcas.docx", package = "IPCASreporter")) %>%
        officer::body_replace_text_at_bkm("employee_name", 
                                          ifelse(isTruthy(identification$employee_name), 
                                                 identification$employee_name, 
                                                 "")) %>%
        officer::body_replace_text_at_bkm("department",
                                          ifelse(isTruthy(identification$department), 
                                                 identification$department, 
                                                              "")) %>%
        officer::body_replace_text_at_bkm("fte",
                                          ifelse(isTruthy(identification$fte), 
                                                 as.character(identification$fte), 
                                                 "")) %>%
        add_doc_section("comment",
                        identification$comment) %>% 
        add_doc_f_section("pubsB",
                          filter_pub_type(section_i$publist,
                                          "B")) %>% 
        add_doc_f_section("pubsM",
                          filter_pub_type(section_i$publist,
                                          "[M|C]")) %>% 
        add_doc_f_section("pubsJ",
                          filter_pub_type(section_i$publist,
                                          "J")) %>% 
        add_doc_f_section("pubsE",
                          filter_pub_type(section_i$publist,
                                          "E")) %>% 
        add_doc_f_section("pubsR",
                          filter_pub_type(section_i$publist,
                                          "R")) %>% 
        add_doc_f_section("pubsT",
                          filter_pub_type(section_i$publist,
                                          "OT")) %>% 
        add_doc_f_section("pubsO",
                          filter_pub_type(section_i$publist,
                                          "O[^T]")) %>% 
        add_doc_f_section("events",
                          section_ii$eventlist) %>% 
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
        add_doc_section("popevents",
                        section_vi_popular$events) %>% 
        add_doc_section("school",
                        section_vi_school$events) %>% 
        add_doc_section("media",
                        section_vi_media$media) %>% 
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
        add_doc_section("pubsRed",
                        section_ix_editions$editions) %>% 
        add_doc_section("section_x",
                        section_x$wip) %>% 
        add_doc_section("section_xi",
                        section_xi$data)
        
        
        
}
