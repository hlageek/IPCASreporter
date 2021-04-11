get_asep <- function(asep_code) {
    # httr::GET(
    #     paste0("https://asep.lib.cas.cz/arl-cav/cs/vysledky/?st=feed&feed=rss&sort=DEFAULT&field=T001N&boolop1=and&term=", asep_code)
    #     ) %>% 
    #     httr::content() %>% 
    #     xml2::xml_find_first("//item//title") %>% 
    #     xml2::xml_text() 
    
 asep_citation <- rvest::read_html(paste0("https://asep.lib.cas.cz/arl-cav/cs/detail/?zf=CAV_BIBCIT_INI&idx=cav_un_epca*", asep_code)) %>% rvest::html_element("div.zf-empty")
 
 emphasis_citation <- asep_citation %>% 
     rvest::html_element("em") %>% 
     rvest::html_text2()
 
 asep_citation %>% 
     rvest::html_text2() %>% 
     stringr::str_replace(emphasis_citation, paste0("<em>", emphasis_citation, "</em>"))
    
}