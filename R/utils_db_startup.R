CREATE_PERSONS_SQL <- "
CREATE TABLE persons (
     person_id INT PRIMARY KEY
,    name_first VARCHAR(50)
,    name_last VARCHAR(50)
,    fte DECIMAL (3,2)
,    email VARCHAR(70)
,    comment TEXT
);
"
CREATE_DEPARTMENTS_SQL <- "
CREATE TABLE departments (
     membership_id INT AUTO_INCREMENT PRIMARY KEY
,    person_id_departments INT UNIQUE
,    department VARCHAR(100)
,    CONSTRAINT `person_id_departments`
        FOREIGN KEY (person_id_departments) REFERENCES persons (person_id)
);
"
CREATE_PUBS_SQL <- "
CREATE TABLE pubs (
     pub_id INT AUTO_INCREMENT PRIMARY KEY
,    person_id_pubs INT
,    pub TEXT
,    CONSTRAINT `person_id_pubs`
        FOREIGN KEY (person_id_pubs) REFERENCES persons (person_id)
);
"
CREATE_EVENTS_SQL <- "
CREATE TABLE events (
     event_id INT AUTO_INCREMENT PRIMARY KEY
,    person_id_events INT
,    event TEXT
,    CONSTRAINT `person_id_events`
        FOREIGN KEY (person_id_events) REFERENCES persons (person_id)
);
"

clear_db <- function(pool) {
    
    tables <- pool::dbListTables(pool)
    purrr::walk(tables, .f = function(x) {
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
}

# create_db_schema(pool)