## code to prepare `universities` dataset goes here
require(dplyr)

uni_raw <-
    readr::read_tsv(
        file = here::here(
            "data-raw/Export_SP_SO_2021_09_17_08_56_48/ExportSP_SO-Table 1.tsv"
        )
    )

universities <- uni_raw %>% select(
    university = 6,
    faculty = 7,
    type = 4,
    disc_program = 3
) %>% 
    mutate(type = stringr::str_replace(type, "navazující ", ""),
           disc_program = stringr::str_replace(disc_program, " \\(dvouoborové\\)", ""),
           disc_program = stringr::str_replace(disc_program, " \\(jednooborové\\)", "")) %>% 
    distinct()


usethis::use_data(as.data.frame(universities), overwrite = TRUE)


