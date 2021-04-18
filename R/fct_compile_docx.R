# compile_docx <- function(data) {
# 
    # officer::read_docx(here::here("inst", "app", "www", "annual_report_ipcas.docx")) %>%
    #     officer::cursor_bookmark("pubs") %>%
    #     body_add_par_n(format_html_citation(data$pubs))
# 
# }

compile_docx <- function(data) {

    test <- officer::read_docx(here::here("inst", "app", "www", "annual_report_ipcas.docx")) %>%
        officer::body_replace_text_at_bkm("employee_name", data$employee_name) %>%
        officer::body_replace_text_at_bkm("department",data$department) %>%
        officer::body_replace_text_at_bkm("fte", data$fte) %>%
        officer::cursor_bookmark("pubs") %>% 
        officer::body_add_par("") %>% 
        body_add_par_n(format_html_citation(data$pubs))
    
    # %>%
    #     officer::body_replace_text_at_bkm("pubs", "")
}
# compile_docx <- function(data) {
# 
#     officer::read_docx(here::here("inst", "app", "www", "annual_report_ipcas.docx")) %>%
#         officer::body_replace_text_at_bkm("employee_name", data$employee_name) %>%
#         officer::body_replace_text_at_bkm("department",data$department) %>%
#         officer::body_replace_text_at_bkm("fte", data$fte) %>%
#         officer::cursor_bookmark("pubs") %>%  add_blocks(format_html_citation(data$pubs)) %>%
#         officer::body_replace_text_at_bkm("pubs", "")
# }