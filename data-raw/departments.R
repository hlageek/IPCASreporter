## code to prepare `departments` dataset goes here


departments <- enframe(
    c(
        "Oddělení analytické filosofie",
        "Oddělení současné kontinentální filosofie",
        "Oddělení logiky",
        "Oddělení pro studium antického a středověkého myšlení",
        "Oddělení pro komeniologii a intelektuální dějiny raného novověku",
        "Oddělení pro studium novověké racionality",
        "Oddělení pro studium moderní české filosofie",
        "Centrum pro teoretická studia",
        "Centrum globálních studií",
        "Kabinet pro studium vědy, techniky a společnosti",
        "Kabinet pro klasická studia",
        "Centrum medievistických studií",
        "Archiv Jana Patočky"
    ),
name = NULL,
value = "department_name"
)

usethis::use_data(departments, overwrite = TRUE)
