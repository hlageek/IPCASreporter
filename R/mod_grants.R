#' grants UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_grants_ui <- function(id){
  ns <- NS(id)
  
  fluidRow(column(width = 6,
 
    textInput(ns("grant_number"), label = "Číslo projektu"),
    
    
    textInput(ns("grant_title"), label = "Název_projektu"),
    
    selectInput(ns("grant_provider"),
                label = "Poskytovatel",
                selected = "",
                choices = 
                c("",
                  "GAČR",
                  "TAČR",
                  "MŠMT")),
    
    selectInput(ns("grant_date_from"), label = "Doba řešení od",
                selected = "",
                choices = c("",
                            seq(2010, 2030, 1))),
    
    selectInput(ns("grant_date_to"), label = "Doba řešení do",
                selected = "",
                choices = c("",
                            seq(2010, 2030, 1))),
    conditionalPanel(
      condition = "input.grant_date_from == output.current_year",
      
      textAreaInput(ns("annotation_cze"), label = "Anotace česky"),
      textAreaInput(ns("annotation_eng"), label = "Anotace anglicky"),

    ),
    
    radioButtons(ns("funding_status"), 
                 label = "Kategorie", 
                 choices = c("Přijatý k financování" = "funded",
                             "Nepřijatý k financování" = "unfunded")
                 ),
    
    actionButton(ns("add"),
                 label = "Add to report"
                 )
    
    ),
    
      column(width = 6,
             
             h3("Řešené či spoluřešené granty"),

             htmlOutput(ns("section_iv_funded"), inline = FALSE),
             
             selectInput(ns("remove_list_funded"), 
                         label = "Item",
                         choices = ""),
             actionButton(ns("remove_funded"),
                          label = "Remove item from report"
                          ),
             
             h3("Projekty podané a nepřijaté k financování"),
             
             htmlOutput(ns("section_iv_unfunded"), inline = FALSE),
             
             selectInput(ns("remove_list_unfunded"), 
                         label = "Item",
                         choices = ""),
             actionButton(ns("remove_unfunded"),
                          label = "Remove item from report"
                          )
             )
      
  )
}
    
#' grants Server Function
#'
#' @noRd 
mod_grants_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
 output$current_year <- renderText({format(Sys.time(), "%Y")})
 outputOptions(output, "current_year", suspendWhenHidden = FALSE)
 

 section_iv <- reactiveValues()

 
 items <- c(
   "grant_number",
   "grant_title",
   "grant_provider",
   "grant_date_from",
   "grant_date_to",
   "annotation_cze",
   "annotation_eng"
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
 
 item_values <- reactive({
   
   unlist(purrr::map(reactiveValuesToList(input)[items], as.character))
   
 })
 
 
 observeEvent(input$add, {
   
   all_items <- list()
   
   for (i in seq_along(items)) {
     
     all_items <- c(all_items, paste(item_names[i], item_values()[i]))
     
   }
   
   
   if (input$funding_status == "funded") {
     
     section_iv$funded[[
       length(
         section_iv$funded)+1]] <- paste(c(all_items,"<br>"), collapse = "<br>")
     
     updateSelectInput(session = session,
                       "remove_list_funded", 
                       choices = seq_along(section_iv$funded)
     )
     
   } else {
     
     section_iv$unfunded[[as.character(
       length(
         section_iv$unfunded)+1)
     ]] <- paste(c(all_items,"<br>"), collapse = "<br>")
     
     updateSelectInput(session = session,
                       "remove_list_unfunded", 
                       choices = seq_along(section_iv$unfunded)
     )
     
   }
   
   
   
 })
 
 observeEvent(input$remove_funded, {
   
   
   section_iv$funded[as.integer(input$remove_list_funded)] <- NULL 
   
   
   updateSelectInput(session = session,
                     "remove_list_funded", 
                     choices = seq_along(section_iv$funded)
                     
   )
   
 })
 
 observeEvent(input$remove_unfunded, {
   
   
   section_iv$unfunded[as.integer(input$remove_list_unfunded)] <- NULL 
   
   
   updateSelectInput(session = session,
                     "remove_list_unfunded", 
                     choices = seq_along(section_iv$unfunded)
                     
   )
   
 })
 
 output$section_iv_funded <- renderText({
   if (length(section_iv$funded)>0) {
     paste(paste0(seq_along(section_iv$funded), ".<br>"),
           section_iv$funded)
   } else {""}
 })
 
 output$section_iv_unfunded <- renderText({
   if (length(section_iv$unfunded)>0) {
     paste(paste0(seq_along(section_iv$unfunded), ".<br>"),
           section_iv$unfunded)
   } else {""}
 })
 
  
  return(section_iv)
    
    
  })}
    
## To be copied in the UI
# mod_grants_ui("grants_ui_1")
    

 
