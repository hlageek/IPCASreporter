## code to prepare `universities` dataset goes here
require(dplyr)
# https://regvssp.msmt.cz/registrvssp/
uni_raw <-
    readr::read_tsv(
        file = here::here(
            "data-raw/Export_SP_SO_2023_11_16_01_17_38/ExportSP_SO-Table 1.tsv"        )
    )

universities_tbl <- uni_raw %>% select(
    university = 6,
    faculty = 7,
    type = 4,
    disc_program = 3
) %>% 
    mutate(type = stringr::str_replace(type, "navazující ", ""),
           disc_program = stringr::str_replace(disc_program, " \\(dvouoborové\\)", ""),
           disc_program = stringr::str_replace(disc_program, " \\(jednooborové\\)", "")) %>% 
    distinct()

universities <- tibble::as_tibble(universities_tbl)
usethis::use_data(universities, overwrite = TRUE)


