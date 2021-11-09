## code to prepare `departments` dataset goes here


departments <- tibble::tribble(
    ~department_name, ~department_abbrev, ~head_email,
        "Oddělení analytické filosofie", "OAF", "novotna@flu.cas.cz",
        "Oddělení současné kontinentální filosofie", "OSKF", "novotna@flu.cas.cz",
        "Oddělení logiky","OL", "novotna@flu.cas.cz",
        "Oddělení pro studium antického a středověkého myšlení","OSASM", "novotna@flu.cas.cz",
        "Oddělení pro komeniologii a intelektuální dějiny raného novověku", "OKIDRN", "novotna@flu.cas.cz",
        "Oddělení pro studium novověké racionality", "OSNR", "novotna@flu.cas.cz",
        "Oddělení pro studium moderní české filosofie", "OSMCF","novotna@flu.cas.cz",
        "Centrum pro teoretická studia", "CTS","novotna@flu.cas.cz",
        "Centrum globálních studií", "CGS","novotna@flu.cas.cz",
        "Kabinet pro studium vědy, techniky a společnosti", "KVTS", "novotna@flu.cas.cz",
        "Kabinet pro klasická studia", "KKS", "novotna@flu.cas.cz",
        "Centrum medievistických studií", "CMS", "novotna@flu.cas.cz",
        "Archiv Jana Patočky", "AJP","novotna@flu.cas.cz"
    )

usethis::use_data(departments, overwrite = TRUE)
