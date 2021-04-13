get_asep <- function(asep_code) {
    # httr::GET(
    #     paste0("https://asep.lib.cas.cz/arl-cav/cs/vysledky/?st=feed&feed=rss&sort=DEFAULT&field=T001N&boolop1=and&term=", asep_code)
    #     ) %>% 
    #     httr::content() %>% 
    #     xml2::xml_find_first("//item//title") %>% 
    #     xml2::xml_text() 
    
 asep_citation <- rvest::read_html(paste0("https://asep.lib.cas.cz/arl-cav/cs/detail/?zf=CAV_BIBCIT_INI&idx=cav_un_epca*", asep_code)) %>% rvest::html_element("div.zf-empty")
 
 if (class(asep_citation) != "xml_missing") {
 emphasis_citation <- asep_citation %>% 
     rvest::html_element("em") %>% 
     rvest::html_text2()
 
 asep_citation %>% 
     rvest::html_text2() %>% 
     stringr::str_replace(emphasis_citation, paste0("<em>", emphasis_citation, "</em>")) %>% 
     trimws() 
 
 } else {
     "No data found for the ASEP item code."
     }
    
}


############################
# word formatting util

format_html_citation <- function(html_citation) {

    citation <- unlist(strsplit(html_citation, "</?em>"))
    fp_normal <- officer::fp_text()
    fp_italic <- update(fp_normal, italic = TRUE)
    
    formatted_citation <- officer::fpar(
        officer::ftext(citation[1], fp_normal), 
        officer::ftext(citation[2], fp_italic),
        officer::ftext(citation[3], fp_normal) 
          
             )
    
}