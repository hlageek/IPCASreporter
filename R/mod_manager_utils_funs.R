present_table <-
    function(ipcas_db,
             tbl,
             person_id,
             tbl_id,
             filter_col = NULL,
             filter_val = NULL,
             names_df,
             person_id_selected,
             dpt_people,
             year = NULL) {
        person_id_tbl <- paste0("person_id_", tbl)
        
        people_df <- dplyr::tbl(ipcas_db, tbl) %>%
            dplyr::filter(.data[[paste0("person_id_", tbl)]] %in% person_id) %>%
            dplyr::filter(.data[[paste0("person_id_", tbl)]] %in% person_id_selected) %>%
            dplyr::select(.data[[paste0("person_id_", tbl)]], tbl_id) %>%
            dplyr::collect()  %>%
            dplyr::rename("person_id" = person_id_tbl)
        
       
        data_input <- transform_table(
            ipcas_db = ipcas_db,
            person_id = person_id,
            tbl = tbl,
            tbl_id = tbl_id,
            filter_col = filter_col,
            filter_val = filter_val,
            names_df = names_df,
            year = year
        )
        
        data_output <- people_df %>%
            dplyr::inner_join(data_input, by = tbl_id) %>%
            dplyr::inner_join(dpt_people, by = c("person_id")) %>%
            dplyr::bind_rows(
                dpt_people %>%
                    dplyr::filter(person_id %in% person_id_selected) %>%
                    dplyr::anti_join(people_df, by = "person_id")
            ) %>%
            dplyr::arrange(name_last, person_id)  %>%
            tidyr::unite("name", c(name_first, name_last), sep = " ") %>%
            dplyr::mutate(name = paste0("<h6>", name, "</h6>")) %>%
            dplyr::mutate(dplyr::across(where(is.character), tidyr::replace_na, "")) %>% 
            dplyr::mutate(name = ifelse(person_id == dplyr::lag(person_id,default = 0), "<br><br>", name))
    }

names_df_switch <- function(x) {
    switch(
        x,
        
        pubs = tibble::tibble(key = c("pub"),
                              names = c("")),
        events = tibble::tibble(key = c("event"),
                              names = c("")),
        undergrad = tibble::tibble(
            key = c(
                "undergrad_school",
                "undergrad_faculty",
                "undergrad_program",
                "undergrad_year",
                "undergrad_level",
                "undergrad_course",
                "undergrad_type_prednasky",
                "undergrad_type_seminare",
                "undergrad_type_cviceni",
                "undergrad_type_vedeni",
                "undergrad_type_texty",
                "undergrad_hours",
                "undergrad_other"
            ),
            names = c(
                "Název VŠ:",
                "Název fakulty:",
                "Název studijního programu/oboru:",
                "Akademický rok, semestr:",
                "Typ studijního programu/oboru:",
                "Název předmětu:",
                "Přednášky:",
                "Semináře:",
                "Cvičení:",
                "Vedení bakalářských a diplomových prací:",
                "Učební texty:",
                "Počet odučených hodin:",
                "Jiné:"
            )  
        ),
        postgrad = tibble::tibble(
            
            key = c(
                "postgrad_school",
                "postgrad_faculty",
                "postgrad_program",
                "postgrad_year",
                "postgrad_level",
                "postgrad_course",
                "postgrad_type_prednasky",
                "postgrad_type_seminare",
                "postgrad_type_cviceni",
                "postgrad_type_vedeni",
                "postgrad_type_texty",
                "postgrad_hours",
                "postgrad_other"
            ),
            names = c(
                "Název VŠ:",
                "Název fakulty:",
                "Název studijního programu/oboru:",
                "Akademický rok, semestr:",
                "Typ studijního programu/oboru:",
                "Název předmětu:",
                "Přednášky:",
                "Semináře:",
                "Cvičení:",
                "Vedení dizertačních prací:",
                "Učební texty:",
                "Počet odučených hodin:",
                "Jiné:"
            )
        ),
        conference = tibble::tibble(
            key = c(
                "conference_contribution",
                "conference_organizer",
                "conference_name",
                "conference_date",
                "conference_location"
            ),
            names = c(
                "Název příspěvku:",
                "Pořadatel:",
                "Název konference:",
                "Datum konání:",
                "Místo konání:"
            )
            
        ),
        lecture = tibble::tibble(
            key = c(
                "lecture_contribution",
                "lecture_organizer",
                "lecture_name",
                "lecture_date",
                "lecture_location"
            ),
            names = c(
                "Název přednášky:",
                "Pořadatel:",
                "Název akce:",
                "Datum konání:",
                "Místo konání:"
            )
        ),
        grant = tibble::tibble(
            key = c(
                "grant_number",
                "grant_title",
                "grant_provider",
                "grant_date_from",
                "grant_date_to",
                "grant_annotation_cze",
                "grant_annotation_eng"
            ),
            names = c(
                "Číslo projektu:",
                "Název projektu:",
                "Poskytovatel:",
                "Doba řešení od:",
                "Doba řešení do:",
                "Anotace česky:",
                "Anotace anglicky:"
            )
        ),
        
        av21 = tibble::tribble(
            ~key,                       ~names,
            "av21_program",            "Program Strategie AV21:",
            "av21_activity",           "Název aktivity (projektu)",
            "av21_person",             "Řešitel aktivity (projektu):",
            "av21_annotation_cze",     "Anotace česky:",      
            "av21_annotation_eng",     "Anotace anglicky:",      
            "av21_results",            "Výstupy (včetně příp. odkazu na ASEP):",
            "av21_partner",            "Spolupracující instituce:"
        ),
        
        popular = tibble::tribble(
            ~key,                            ~names,
            "popular_contribution",          "Název akce:",
            "popular_description",           "Popis aktivity:",
            "popular_organizer_primary",     "Hlavní pořadatel:", 
            "popular_organizer_secondary",   "Spolupořadatel:",
            "popular_place",                 "Místo konání akce:",
            "popular_date",                  "Datum konání akce:"
        ),
        
        school = tibble::tribble(
            ~key,                            ~names,
            "school_contribution",          "Název přednášky či specifikace jiného druhu akce:",           
            "school_name",                  "Pořadatel/škola:",   
            "school_description",           "Popis činnosti:"         
        ),    
        
        media = tibble::tribble(
            ~key,                            ~names,
            "media_contribution",            "Název:",               
            "media_name",                    "Médium:",       
            "media_description",             "Doplňující informace:"                
        ),  
        
        gov = tibble::tribble(
            ~key,                            ~names,
            "gov_body",                      "Instituce státní nebo veřejné správy:",    
            "gov_description",               "Popis spolupráce:"                       
        ),  
        
        int_projects = tibble::tribble(
            ~key,                            ~names,
            "int_projects_name",             "Název projektu:"                   
        ), 
        
        int_bilateral = tibble::tribble(
            ~key,                            ~names,
            "int_bilateral_description",     "Bilaterální spolupráce:"                   
        ), 
        
        other_awards = tibble::tribble(
            ~key,                            ~names,
            "other_awards_description",     "Název ocenění:"                   
        ), 
        
        other_member = tibble::tribble(
            ~key,                            ~names,
            "other_member_name",            "Grémium:",   
            "other_member_institute",       "Organizace:",        
            "other_member_position",        "Funkce:"
        ), 
        
        other_editions = tibble::tribble(
            ~key,                            ~names,
            "other_editions_name",           "Název:",  
            "other_editions_description",    "Doplňující informace:"                        
        ), 
        
        other_reviews = tibble::tibble(
            key = c("other_reviews_name",
                    "other_reviews_description"),
            names = c("Název:",
                      "Doplňující informace:")
            
        ),
        
        wip = tibble::tribble(
            ~key,                            ~names,
            "wip_description",               "Popis:"
        ), 
        
        various = tibble::tribble(
            ~key,                            ~names,
            "various_description",           "Popis:"                        
        ), 
        
        
        
        
    )
}
