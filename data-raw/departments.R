## code to prepare `departments` dataset goes here


departments <- tibble::tribble(
    ~department_name, ~department_abbrev, ~head_email,
        "Oddělení analytické filosofie", "OAF", "hladik@flu.cas.cz",
        "Oddělení současné kontinentální filosofie", "OSKF", "hladik@flu.cas.cz",
        "Oddělení logiky","OL", "hladik@flu.cas.cz",
        "Oddělení pro studium antického a středověkého myšlení","OSASM", "hladik@flu.cas.cz",
        "Oddělení pro komeniologii a intelektuální dějiny raného novověku", "OKIDRN", "hladik@flu.cas.cz",
        "Oddělení pro studium novověké racionality", "OSNR", "hladik@flu.cas.cz",
        "Oddělení pro studium moderní české filosofie", "OSMCF","hladik@flu.cas.cz",
        "Centrum pro teoretická studia", "CTS","hladik@flu.cas.cz",
        "Centrum globálních studií", "CGS","hladik@flu.cas.cz",
        "Kabinet pro studium vědy, techniky a společnosti", "KVTS", "hladik@flu.cas.cz",
        "Kabinet pro klasická studia", "KKS", "hladik@flu.cas.cz",
        "Centrum medievistických studií", "CMS", "hladik@flu.cas.cz",
        "Archiv Jana Patočky", "AJP","hladik@flu.cas.cz"
    )

usethis::use_data(departments, overwrite = TRUE)
