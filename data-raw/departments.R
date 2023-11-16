## code to prepare `departments` dataset goes here


departments <- tibble::tribble(
    ~department_name, ~department_abbrev, ~head_email, ~head_id,
        "Oddělení analytické filosofie", "OAF", "marvan@flu.cas.cz", "29001700",
        "Oddělení aplikované filosofie a etiky", "OAFE","novotna@flu.cas.cz",
        "Oddělení logiky","OL", "puncochar@flu.cas.cz", "29003841",
        "Oddělení pro komeniologii a intelektuální dějiny raného novověku", "OKIDRN", "havelka@flu.cas.cz", "29000942",
        "Oddělení pro studium antického a středověkého myšlení","OSASM", "petr.dvorak@flu.cas.cz", "29000525",
        "Oddělení pro studium moderní české filosofie", "OSMCF","landa@flu.cas.cz", "29000198",
        "Oddělení pro studium novověké racionality", "OSNR", "jchotas@flu.cas.cz", "29001102",
        "Oddělení současné kontinentální filosofie", "OSKF", "nitsche@flu.cas.cz", "29002038",
        "Oddělení politické filosofie a výzkumu globalizace", "OPFVG","novotna@flu.cas.cz",
        "Centrum medievistických studií", "CMS", "zurek@flu.cas.cz",
        "Centrum pro teoretická studia", "CTS", "frei@cts.cuni.cz", "29000718",
        "Kabinet pro klasická studia", "KKS", "novotna@flu.cas.cz",
        "Kabinet pro studium vědy, techniky a společnosti", "KVTS", "novotna@flu.cas.cz",
        "Archiv Jana Patočky", "AJP", "frei@cts.cuni.cz", "29000718"
    )

usethis::use_data(departments, overwrite = TRUE)
