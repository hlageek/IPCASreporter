## code to prepare `departments` dataset goes here


departments <- tibble::tribble(
    ~department_name, ~department_abbrev, ~head_email, ~head_id,
        "Oddělení analytické filosofie", "OAF", "marvan@flu.cas.cz", "29001700",
        "Oddělení aplikované filosofie a etiky", "OAFE","hribek@flu.cas.cz", "29000952",
        "Oddělení logiky","OL", "puncochar@flu.cas.cz", "29003841",
        "Oddělení pro komeniologii a intelektuální dějiny raného novověku", "OKIDRN", "havelka@flu.cas.cz", "29000942",
        "Oddělení pro studium antického a středověkého myšlení","OSASM", "petr.dvorak@flu.cas.cz", "29000525",
        "Oddělení pro studium moderní české filosofie", "OSMCF","landa@flu.cas.cz", "29000198",
        "Oddělení pro studium novověké racionality", "OSNR", "jchotas@flu.cas.cz", "29001102",
        "Oddělení současné kontinentální filosofie", "OSKF", "nitsche@flu.cas.cz", "29002038",
        "Oddělení politické filosofie a výzkumu globalizace", "OPFVG", "lansky@flu.cas.cz", "29001607",
        "Centrum medievistických studií", "CMS", "soukup@flu.cas.cz", "29002870",
        "Centrum pro teoretická studia", "CTS", "frei@cts.cuni.cz", "29000718",
        "Kabinet pro klasická studia", "KKS", "kitzler@ics.cas.cz", "29000020",
        "Kabinet pro studium vědy, techniky a společnosti", "KVTS", "balon@flu.cas.cz", "29000204",
        "Archiv Jana Patočky", "AJP", "frei@cts.cuni.cz", "29000718"
    )

usethis::use_data(departments, overwrite = TRUE)
