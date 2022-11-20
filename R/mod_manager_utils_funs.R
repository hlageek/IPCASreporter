present_table <-
    function(ipcas_db,
             tbl,
             person_id,
             tbl_id,
             filter_col = NULL,
             filter_val = NULL,
             names_df,
             person_id_selected,
             dpt_people) {
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
            names_df = names_df
        )
        
        
        data_output <- people_df %>%
            dplyr::inner_join(data_input, by = tbl_id) %>%
            dplyr::inner_join(dpt_people, by = c("person_id")) %>%
            dplyr::bind_rows(
                dpt_people %>%
                    dplyr::filter(person_id %in% person_id_selected) %>%
                    dplyr::anti_join(people_df, by = "person_id")
            ) %>%
            dplyr::arrange(name_last)  %>%
            tidyr::unite("name", c(name_first, name_last), sep = " ") %>%
            dplyr::mutate(name = paste0("<h6>", name, "</h6>")) %>%
            dplyr::mutate(dplyr::across(where(is.character), tidyr::replace_na, ""))
    }

names_df_switch <- function(x) {
    switch(
        x,
        
        pubs = tibble::tibble(key = c("pub"),
                              names = c("")),
        conference_domestic = tibble::tibble(
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
        other_reviews = tibble::tibble(
            key = c("other_reviews_name",
                    "other_reviews_description"),
            names = c("Název:",
                      "Doplňující informace:")
            
        ),
        
        
        
        
    )
}
