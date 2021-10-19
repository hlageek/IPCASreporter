## code to prepare `providers` dataset goes here

providers <- tibble::enframe( 
    c("Akademie věd České republiky",
               "Bezpečnostní informační služba",
               "Český báňský úřad",
               "Český statistický úřad",
               "Český úřad zeměměřický a katastrální",
               "Grantová agentura České republiky",
               "Královéhradecký kraj",
               "Hlavní město Praha",
               "Jihočeský kraj",
               "Jihomoravský kraj",
               "Karlovarský kraj",
               "Liberecký kraj",
               "Moravskoslezský kraj",
               "Olomoucký kraj",
               "Pardubický kraj",
               "Plzeňský kraj",
               "Středočeský kraj",
               "Ústecký kraj",
               "Kraj Vysočina",
               "Zlínský kraj",
               "Ministerstvo dopravy",
               "Ministerstvo financí",
               "Ministerstvo kultury",
               "Ministerstvo pro místní rozvoj",
               "Ministerstvo obrany",
               "Ministerstvo průmyslu a obchodu",
               "Ministerstvo práce a sociálních věcí",
               "Ministerstvo spravedlnosti",
               "Ministerstvo školství, mládeže a tělovýchovy",
               "Ministerstvo vnitra",
               "Ministerstvo zdravotnictví",
               "Ministerstvo zemědělství",
               "Ministerstvo životního prostředí",
               "Ministerstvo zahraničních věcí",
               "Národní bezpečnostní úřad",
               "Státní úřad pro jadernou bezpečnost",
               "Technologická agentura ČR",
               "Úřad vlády České republiky",
               "Územně samosprávné celky (nerozlišeno)"),
    name = NULL,
    value = "providers"
)

usethis::use_data(providers, overwrite = TRUE)


