

compile_docx <- function(data) {
    
    officer::read_docx(here::here("inst", "app", "www", "annual_report_ipcas.docx")) %>%
        officer::body_replace_text_at_bkm("employee_name", data$employee_name) %>%
        officer::body_replace_text_at_bkm("department",data$department) %>%
        officer::body_replace_text_at_bkm("fte", data$fte) %>%
        officer::cursor_bookmark("pubs") %>% 
        officer::body_add_par("") %>% 
        body_add_par_n(format_html_citation(data$pub)) %>%
        officer::body_replace_text_at_bkm("pubs", "")
}
