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
     membership_id INT AUTO_INCREMENT PRIMARY KEY
,    person_id_departments INT UNIQUE
,    department VARCHAR(100)
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

clear_db <- function(pool) {
    
    tables <- pool::dbListTables(pool)
    tables_reordered <- c(tables[tables != "persons"], "persons")
    purrr::walk(tables_reordered, .f = function(x) {
        pool::dbRemoveTable(pool, x)
        }
    )
    
}

# clear_db(pool)

create_db_schema <- function(pool){
    # TODO: Full DB structure
    DBI::dbExecute(pool, CREATE_PERSONS_SQL)
    DBI::dbExecute(pool, CREATE_DEPARTMENTS_SQL)
    DBI::dbExecute(pool, CREATE_PUBS_SQL)
    DBI::dbExecute(pool, CREATE_EVENTS_SQL)
    DBI::dbExecute(pool, CREATE_UNDERGRAD_SQL)
}

# create_db_schema(pool)