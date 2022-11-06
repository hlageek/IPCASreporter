#' grants UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_grants_ui <- function(id, i18n){
  ns <- NS(id)
  
  fluidRow(column(width = 6,
 
    textInput(ns("grant_number"), label = i18n$t("Číslo projektu") ),
    
    
    textInput(ns("grant_title"), label = i18n$t("Název projektu") ),
    
    selectInput(ns("grant_provider"),
                label = i18n$t("Poskytovatel") ,
                selected = "",
                choices =  c("", IPCASreporter::providers$providers, "Jiný/Other" = "other")),
    
    conditionalPanel(
       condition = 'input.grant_provider == "other"',
       ns = ns,
       
       tags$div(
       textInput(ns("bespoke_provider"), label = i18n$t("Jiný poskytovatel") ),
       style="display:inline-block"),
       tags$div(
       actionButton(ns("add_bespoke"), icon = icon("plus"), label = ""),
       style="display:inline-block", title = i18n$t("Add provider") )
       
       ),
    
    selectInput(ns("grant_date_from"), label = i18n$t("Doba řešení od") ,
                selected = "",
                choices = c("",
                            seq(2010, 2030, 1))),
    
    selectInput(ns("grant_date_to"), label = i18n$t("Doba řešení do") ,
                selected = "",
                choices = c("",
                            seq(2010, 2030, 1))),
    conditionalPanel(
      condition = "input.grant_date_from == output.current_year",
      ns = ns,
      
      textAreaInput(ns("grant_annotation_cze"), label = i18n$t("Anotace česky") ,
                    placeholder = i18n$t("Pro nově zahájené projekty.") ),
      textAreaInput(ns("grant_annotation_eng"), label = i18n$t("Anotace anglicky") ,
                    placeholder = i18n$t("Pro nově zahájené projekty.") ),

    ),
    
    radioButtons(ns("grant_funding_status"), 
                 label = i18n$t("Kategorie: ") , 
                 choices = c("Řešený, nebo přijatý k financování"  = "funded",
                             "Posuzovaný, nebo nepřijatý k financování"  = "unfunded")
                 ),
    
   br(), br(), br(),
   
    actionButton(ns("add"),
                 label = i18n$t("Zadat do výkazu") ,                  icon = icon("check"),                  class = "btn-success"
                 )
    
    ),
    
      column(width = 6,
             
             h3(i18n$t("Řešené či spoluřešené granty") ),

             htmlOutput(ns("section_iv_funded"), inline = FALSE),
             
             selectInput(ns("remove_list_funded"), 
                         label = i18n$t("Položka") ,
                         choices = ""),
             actionButton(ns("remove_funded"),
                          label = i18n$t("Odstranit z výkazu") , class = "btn-primary", icon = icon("trash")
                          ),
             
             h3(i18n$t("Projekty posuzované, nebo nepřijaté k financování") ),
             
             htmlOutput(ns("section_iv_unfunded"), inline = FALSE),
             
             selectInput(ns("remove_list_unfunded"), 
                         label = i18n$t("Položka") ,
                         choices = ""),
             actionButton(ns("remove_unfunded"),
                          label = i18n$t("Odstranit z výkazu") , class = "btn-primary", icon = icon("trash")
                          )
             )
      
  )
}
    
#' grants Server Function
#'
#' @noRd 
mod_grants_server <- function(id, usr, i18n) {
  moduleServer(id, function(input, output, session) {
    
 output$current_year <- renderText({format(Sys.time(), "%Y")})
 outputOptions(output, "current_year", suspendWhenHidden = FALSE)
 

 section_iv <- reactiveValues()
 loc <- reactiveValues()
 
 items <- c(
   "grant_number",
   "grant_title",
   "grant_provider",
   "grant_date_from",
   "grant_date_to",
   "grant_annotation_cze",
   "grant_annotation_eng"
 )
 
 item_names <- c(
   "Číslo projektu:",
   "Název projektu:",
   "Poskytovatel:",
   "Doba řešení od:",
   "Doba řešení do:",
   "Anotace česky:",
   "Anotace anglicky:"
 )
 
 
 loc$names <- tibble::tibble(key = items,
                             names = item_names)
 
 observeEvent(req(input$add_bespoke), {
    
    updateSelectInput(session = session,
                      "grant_provider", 
                      selected = input$bespoke_provider,
                      choices  =  c(input$bespoke_provider, IPCASreporter::providers$providers, "Jiný/Other" = "other")
                      
    )
    
 })
 
 
 # init ####
 observeEvent(usr$person_id, {
     
     loc$funded <- transform_table(ipcas_db = ipcas_db,
                                     person_id = usr$person_id,
                                     tbl = "grants",
                                     tbl_id = "grant_id",
                                     filter_col = "grant_funding_status",
                                     filter_val = "funded",
                                     names_df = loc$names)
     
     loc$unfunded <- transform_table(ipcas_db = ipcas_db,
                                    person_id = usr$person_id,
                                    tbl = "grants",
                                    tbl_id = "grant_id",
                                    filter_col = "grant_funding_status",
                                    filter_val = "unfunded",
                                    names_df = loc$names)
     
     
     ids_funded <-  loc$funded %>% 
         dplyr::pull(grant_id)
     
     section_iv$funded <- paste0("<br>", 
                                 loc$funded$data,
                                 "<br>")
     
     updateSelectInput(session = session,
                       "remove_list_funded",
                       choices = stats::setNames(
                           ids_funded,
                           seq_along(ids_funded)))
     
     ids_unfunded <-  loc$unfunded %>% 
         dplyr::pull(grant_id)
     
     
     section_iv$unfunded <- paste0("<br>", 
                                   loc$unfunded$data,
                                   "<br>")
     
     updateSelectInput(session = session,
                       "remove_list_unfunded",
                       choices = stats::setNames(
                           ids_unfunded,
                           seq_along(ids_unfunded)))
     
 })
 

 # add ####

 observeEvent(input$add, {

          # check and require inputs
          checks <- stats::setNames(item_names, items)
          checks_no_annotation <- checks[!grepl("annotation", names(checks))]
          check_inputs(input, checks_no_annotation, text = i18n()$t("Zadejte") )
          
          if (input$grant_date_from == as.integer( format(Sys.Date(), "%Y"))) {
              checks_annotation <- checks[grepl("annotation", names(checks))]
              check_inputs(input, checks_annotation)
          }
  
     
     all_items <- collect_items(items, input)
     
     new_entry_df <- tibble::tibble(key = items,
                                    value = all_items) %>% 
         tidyr::pivot_wider(tidyselect::everything(),
                            names_from = "key",
                            values_from = "value") %>% 
         dplyr::mutate(person_id_grants = usr$person_id,
                       grant_id_year = as.integer( format(Sys.Date(), "%Y")),
                       grant_funding_status = input$grant_funding_status) 
     
     DBI::dbAppendTable(ipcas_db, "grants", new_entry_df)
     
     if (input$grant_funding_status == "funded") {
         
         

        loc$funded <- transform_table(ipcas_db = ipcas_db,
                                   person_id = usr$person_id,
                                   tbl = "grants",
                                   tbl_id = "grant_id",
                                   filter_col = "grant_funding_status",
                                   filter_val = "funded",
                                   names_df = loc$names)
         
         ids_funded <-  loc$funded %>% 
             dplyr::pull(grant_id)
         
         section_iv$funded <- paste0("<br>", 
                                       loc$funded$data,
                                       "<br>")
         
         updateSelectInput(session = session,
                           "remove_list_funded",
                           choices = stats::setNames(
                               ids_funded,
                               seq_along(ids_funded)))
             
     } else {
         

             loc$unfunded <- transform_table(ipcas_db = ipcas_db,
                                           person_id = usr$person_id,
                                           tbl = "grants",
                                           tbl_id = "grant_id",
                                           filter_col = "grant_funding_status",
                                           filter_val = "unfunded",
                                           names_df = loc$names)
         
         
         ids_unfunded <-  loc$unfunded %>% 
             dplyr::pull(grant_id)
         
         
         section_iv$unfunded <- paste0("<br>", 
                                      loc$unfunded$data,
                                      "<br>")
         
         updateSelectInput(session = session,
                           "remove_list_unfunded",
                           choices = stats::setNames(
                               ids_unfunded,
                               seq_along(ids_unfunded)))
         
     }
   
   
 })
 
 # remove funded ####
 
 observeEvent(input$remove_funded, {
   
     
     
     loc$funded <- loc$funded %>% 
         dplyr::filter(!grant_id %in% req(input$remove_list_funded))
     
     pool::dbExecute(ipcas_db, 
                     "DELETE FROM grants WHERE grant_id IN (?)",
                     params = list(input$remove_list_funded))
   
     ids_funded <-  loc$funded %>% 
         dplyr::pull(grant_id)
     
     section_iv$funded <- paste0("<br>", 
                                 loc$funded$data,
                                 "<br>")
     
     updateSelectInput(session = session,
                       "remove_list_funded",
                       choices = stats::setNames(
                           ids_funded,
                           seq_along(ids_funded)))
   
 })
 
 
 # remove unfunded ####
 
 observeEvent(input$remove_unfunded, {
   
     
     loc$unfunded <- loc$unfunded %>% 
         dplyr::filter(!grant_id %in% req(input$remove_list_unfunded))
     
     pool::dbExecute(ipcas_db, 
                     "DELETE FROM grants WHERE grant_id IN (?)",
                     params = list(input$remove_list_unfunded))
     
     ids_unfunded <-  loc$unfunded %>% 
         dplyr::pull(grant_id)
     
     section_iv$unfunded <- paste0("<br>", 
                                 loc$unfunded$data,
                                 "<br>")
     
     updateSelectInput(session = session,
                       "remove_list_unfunded",
                       choices = stats::setNames(
                           ids_unfunded,
                           seq_along(ids_unfunded)))
   
 })
 
 # output funded ####
 
 output$section_iv_funded <- renderText({
   if (nrow(loc$funded)>0) {
  
       text_to_display <- loc$funded %>% 
           dplyr::pull(data)
       
       paste0(
           paste0(seq_along(text_to_display), ".<br>"),
           text_to_display,
           "<br><br>")
       
   } else {""}
 })
 
 # output funded ####
 
 output$section_iv_unfunded <- renderText({

     if (nrow(loc$unfunded)>0) {
         
         text_to_display <- loc$unfunded %>% 
             dplyr::pull(data)
         
         paste0(
             paste0(seq_along(text_to_display), ".<br>"),
             text_to_display,
             "<br><br>")
         
     } else {""}
 })
 
  return(section_iv)
    
    
  })}
    

