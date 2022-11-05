CREATE_PERSONS_SQL <- "
CREATE TABLE IF NOT EXISTS persons (
     person_id INT PRIMARY KEY
,    name_first VARCHAR(50)
,    name_last VARCHAR(50)
,    fte DECIMAL (3,2)
,    email VARCHAR(70)
,    comment TEXT
);
"
CREATE_DEPARTMENTS_SQL <- "
CREATE TABLE IF NOT EXISTS departments (
     department_id INT AUTO_INCREMENT PRIMARY KEY
,    person_id_departments INT UNIQUE
,    department VARCHAR(200)
,    CONSTRAINT `person_id_departments`
        FOREIGN KEY (person_id_departments) REFERENCES persons (person_id)
);
"
CREATE_PUBS_SQL <- "
CREATE TABLE IF NOT EXISTS pubs (
     pub_id INT AUTO_INCREMENT PRIMARY KEY
,    person_id_pubs INT
,    pub TEXT
,    CONSTRAINT `person_id_pubs`
        FOREIGN KEY (person_id_pubs) REFERENCES persons (person_id)
);
"
CREATE_EVENTS_SQL <- "
CREATE TABLE IF NOT EXISTS events (
     event_id INT AUTO_INCREMENT PRIMARY KEY
,    person_id_events INT
,    event TEXT
,    CONSTRAINT `person_id_events`
        FOREIGN KEY (person_id_events) REFERENCES persons (person_id)
);
"
CREATE_UNDERGRAD_SQL <- "
CREATE TABLE IF NOT EXISTS undergrad (
      undergrad_id INT AUTO_INCREMENT PRIMARY KEY
,     person_id_undergrad INT
,     undergrad_school VARCHAR(500)
,     undergrad_faculty VARCHAR(500)
,     undergrad_program VARCHAR(500)
,     undergrad_year VARCHAR(100)
,     undergrad_level VARCHAR(100)
,     undergrad_course VARCHAR(500)
,     undergrad_type_prednasky VARCHAR(100)
,     undergrad_type_seminare VARCHAR(100)
,     undergrad_type_cviceni VARCHAR(100)
,     undergrad_type_vedeni VARCHAR(100)
,     undergrad_type_texty VARCHAR(100)
,     undergrad_hours INT
,     undergrad_other TEXT
,    CONSTRAINT `person_id_undergrad`
        FOREIGN KEY (person_id_undergrad) REFERENCES persons (person_id)
);
"
CREATE_POSTGRAD_SQL <- "
CREATE TABLE IF NOT EXISTS postgrad (
      postgrad_id INT AUTO_INCREMENT PRIMARY KEY
,     person_id_postgrad INT
,     postgrad_school VARCHAR(500)
,     postgrad_faculty VARCHAR(500)
,     postgrad_program VARCHAR(500)
,     postgrad_year VARCHAR(100)
,     postgrad_level VARCHAR(100)
,     postgrad_course VARCHAR(500)
,     postgrad_type_prednasky VARCHAR(100)
,     postgrad_type_seminare VARCHAR(100)
,     postgrad_type_cviceni VARCHAR(100)
,     postgrad_type_vedeni VARCHAR(100)
,     postgrad_type_texty VARCHAR(100)
,     postgrad_hours INT
,     postgrad_other TEXT
,    CONSTRAINT `person_id_postgrad`
        FOREIGN KEY (person_id_postgrad) REFERENCES persons (person_id)
);
"

CREATE_CONFERENCES_SQL <- "
CREATE TABLE IF NOT EXISTS conferences (
     conference_id INT AUTO_INCREMENT PRIMARY KEY
,    person_id_conferences INT
,    conference_contribution VARCHAR(500)
,    conference_organizer VARCHAR(500)
,    conference_name TEXT
,    conference_date VARCHAR(40)
,    conference_location VARCHAR(25)
,    CONSTRAINT `person_id_conferences`
        FOREIGN KEY (person_id_conferences) REFERENCES persons (person_id)
);
"
CREATE_LECTURES_SQL <- "
CREATE TABLE IF NOT EXISTS lectures (
     lecture_id INT AUTO_INCREMENT PRIMARY KEY
,    person_id_lectures INT
,    lecture_contribution VARCHAR(500)
,    lecture_organizer VARCHAR(500)
,    lecture_name TEXT
,    lecture_date VARCHAR(40)
,    lecture_location VARCHAR(25)
,    CONSTRAINT `person_id_lectures`
        FOREIGN KEY (person_id_lectures) REFERENCES persons (person_id)
);
"
CREATE_GRANTS_SQL <- "
CREATE TABLE IF NOT EXISTS grants (
     grant_id INT AUTO_INCREMENT PRIMARY KEY
,    grant_id_year INT
,    person_id_grants INT
,    grant_number VARCHAR(100)
,    grant_title TEXT
,    grant_provider VARCHAR(500)
,    grant_date_from INT
,    grant_date_to INT
,    grant_annotation_cze TEXT
,    grant_annotation_eng TEXT
,    grant_funding_status VARCHAR(10)
,    CONSTRAINT `person_id_grants`
        FOREIGN KEY (person_id_grants) REFERENCES persons (person_id)
);
"
CREATE_AV21_SQL <- "
CREATE TABLE IF NOT EXISTS av21 (
     av21_id INT AUTO_INCREMENT PRIMARY KEY
,    av21_id_year INT
,    person_id_av21 INT
,    av21_program VARCHAR(250)
,    av21_activity VARCHAR(500)
,    av21_person VARCHAR(250)
,    av21_annotation_cze TEXT
,    av21_annotation_eng TEXT
,    av21_results TEXT
,    av21_partner TEXT
,    CONSTRAINT `person_id_av21`
        FOREIGN KEY (person_id_av21) REFERENCES persons (person_id)
);
"
CREATE_POPULAR_SQL <- "
CREATE TABLE IF NOT EXISTS popular (
     popular_id INT AUTO_INCREMENT PRIMARY KEY
,    popular_id_year INT
,    person_id_popular INT
,    popular_contribution VARCHAR(500)
,    popular_description TEXT
,    popular_organizer_primary VARCHAR(250)
,    popular_organizer_secondary VARCHAR(250)
,    popular_place VARCHAR(250)
,    popular_date VARCHAR(40)
,    CONSTRAINT `person_id_popular`
        FOREIGN KEY (person_id_popular) REFERENCES persons (person_id)
);
"

CREATE_SCHOOL_SQL <- "
CREATE TABLE IF NOT EXISTS school (
     school_id INT AUTO_INCREMENT PRIMARY KEY
,    school_id_year INT
,    person_id_school INT
,    school_contribution VARCHAR(500)
,    school_name VARCHAR(500)
,    school_description TEXT
,    CONSTRAINT `person_id_school`
        FOREIGN KEY (person_id_school) REFERENCES persons (person_id)
);
"

CREATE_MEDIA_SQL <- "
CREATE TABLE IF NOT EXISTS media (
     media_id INT AUTO_INCREMENT PRIMARY KEY
,    media_id_year INT
,    person_id_media INT
,    media_contribution VARCHAR(500)
,    media_name VARCHAR(500)
,    media_description TEXT
,    CONSTRAINT `person_id_media`
        FOREIGN KEY (person_id_media) REFERENCES persons (person_id)
);
"
CREATE_GOV_SQL <- "
CREATE TABLE IF NOT EXISTS gov (
     gov_id INT AUTO_INCREMENT PRIMARY KEY
,    gov_id_year INT
,    person_id_gov INT
,    gov_body VARCHAR(500)
,    gov_description TEXT
,    CONSTRAINT `person_id_gov`
        FOREIGN KEY (person_id_gov) REFERENCES persons (person_id)
);
"

CREATE_INT_PROJECTS_SQL <- "
CREATE TABLE IF NOT EXISTS int_projects (
     int_projects_id INT AUTO_INCREMENT PRIMARY KEY
,    int_projects_id_year INT
,    person_id_int_projects INT
,    int_projects_name VARCHAR(500)
,    CONSTRAINT `person_id_int_projects`
        FOREIGN KEY (person_id_int_projects) REFERENCES persons (person_id)
);
"

CREATE_INT_BILATERAL_SQL <- "
CREATE TABLE IF NOT EXISTS int_bilateral (
     int_bilateral_id INT AUTO_INCREMENT PRIMARY KEY
,    int_bilateral_id_year INT
,    person_id_int_bilateral INT
,    int_bilateral_description VARCHAR(500)
,    CONSTRAINT `person_id_int_bilateral`
        FOREIGN KEY (person_id_int_bilateral) REFERENCES persons (person_id)
);
"


#' @export
clear_db <- function(pool) {
    
    tables <- pool::dbListTables(pool)
    tables_reordered <- c(tables[tables != "persons"], "persons")
    purrr::walk(tables_reordered, .f = function(x) {
        pool::dbRemoveTable(pool, x)
        }
    )
    
}

# clear_db(pool)

#' @export
create_db_schema <- function(pool){
    # TODO: Full DB structure
    DBI::dbExecute(pool, CREATE_PERSONS_SQL)
    DBI::dbExecute(pool, CREATE_DEPARTMENTS_SQL)
    DBI::dbExecute(pool, CREATE_PUBS_SQL)
    DBI::dbExecute(pool, CREATE_EVENTS_SQL)
    DBI::dbExecute(pool, CREATE_UNDERGRAD_SQL)
    DBI::dbExecute(pool, CREATE_POSTGRAD_SQL)
    DBI::dbExecute(pool, CREATE_CONFERENCES_SQL)
    DBI::dbExecute(pool, CREATE_LECTURES_SQL)
    DBI::dbExecute(pool, CREATE_GRANTS_SQL)
    DBI::dbExecute(pool, CREATE_AV21_SQL)
    DBI::dbExecute(pool, CREATE_POPULAR_SQL)
    DBI::dbExecute(pool, CREATE_SCHOOL_SQL)
    DBI::dbExecute(pool, CREATE_MEDIA_SQL)
    DBI::dbExecute(pool, CREATE_GOV_SQL)
    DBI::dbExecute(pool, CREATE_INT_PROJECTS_SQL)
    DBI::dbExecute(pool, CREATE_INT_BILATERAL_SQL)
    
}

# create_db_schema(pool)


make_globals <- quote({
    
    shinymanager::set_labels(
        language = "en",
        "Please authenticate" = "Výroční výkaz (Annual report)",
        "Username:" = "User:",
        "Password:" = "Password:"
    )
    
    ipcas_db <- pool::dbPool(
            drv = RMariaDB::MariaDB(),
            dbname = golem::get_golem_options("dbname"),
            username = golem::get_golem_options("dbusername"),
            password = golem::get_golem_options("dbpassword")
    )
    
    shiny::onStop(function() {
        pool::poolClose(ipcas_db)
    })
    

})
    

#' @export
create_credentials_db <- function(admin_pass, path, df = NULL) {
    # Init DB using credentials data
    # define some credentials
    admin_df <- data.frame(
        user = "admin", # mandatory
        password = admin_pass, # mandatory
        admin = TRUE,
        person_id = 123,
        level = 5,
        stringsAsFactors = FALSE
    )
    
    credentials <- dplyr::bind_rows(admin_df, df)

    # Init the database
    shinymanager::create_db(
        credentials_data = credentials,
        sqlite_path = path, # will be created
        passphrase = admin_pass
    )
    
}

