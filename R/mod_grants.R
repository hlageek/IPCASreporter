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
    
    
    textInput(ns("grant_title"), label = "Název projektu"),
    
    selectInput(ns("grant_provider"),
                label = "Poskytovatel",
                selected = "",
                choices =  c("", "Jiný/Other" = "other")),
    
    conditionalPanel(
       condition = 'input.grant_provider == "other"',
       ns = ns,
       
       tags$div(
       textInput(ns("bespoke_provider"), label = "Jiný poskytovatel"),
       style="display:inline-block"),
       tags$div(
       actionButton(ns("add_bespoke"), icon = icon("plus"), label = ""),
       style="display:inline-block", title = "ddf")
       
       ),
    
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
      ns = ns,
      
      textAreaInput(ns("annotation_cze"), label = "Anotace česky",
                    placeholder = "Pro nově zahájené projekty."),
      textAreaInput(ns("annotation_eng"), label = "Anotace anglicky",
                    placeholder = "For newly launched projects."),

    ),
    
    radioButtons(ns("funding_status"), 
                 label = "Kategorie: ", 
                 choices = c("Řešený, nebo přijatý k financování" = "funded",
                             "Posuzovaný, nebo nepřijatý k financování" = "unfunded")
                 ),
    
   br(), br(), br(),
   
    actionButton(ns("add"),
                 label = "Add to report",                  icon = icon("check"),                  class = "btn-success"
                 )
    
    ),
    
      column(width = 6,
             
             h3("Řešené či spoluřešené granty"),

             htmlOutput(ns("section_iv_funded"), inline = FALSE),
             
             selectInput(ns("remove_list_funded"), 
                         label = "Item",
                         choices = ""),
             actionButton(ns("remove_funded"),
                          label = "Remove from report", class = "btn-primary", icon = icon("trash")
                          ),
             
             h3("Projekty posuzované, nebo nepřijaté k financování"),
             
             htmlOutput(ns("section_iv_unfunded"), inline = FALSE),
             
             selectInput(ns("remove_list_unfunded"), 
                         label = "Item",
                         choices = ""),
             actionButton(ns("remove_unfunded"),
                          label = "Remove from report", class = "btn-primary", icon = icon("trash")
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
 
 
 observeEvent(req(input$add_bespoke), {
    
    updateSelectInput(session = session,
                      "grant_provider", 
                      selected = input$bespoke_provider,
                      choices  =  c(input$bespoke_provider, IPCASreporter::providers$providers, "Jiný/Other" = "other")
                      
    )
    
 })
 
 item_values <- reactive({
   
 unlist(purrr::map(reactiveValuesToList(input)[items], as.character))
   

 })
 
 
 observeEvent(input$add, {
   #browser()
    
    item_values2 <- item_values()
    items2 <- items
    item_names2 <- item_names
    
   if (input$grant_date_from != as.integer( format(Sys.Date(), "%Y"))) {
      
      item_values2 <- item_values2[names(item_values2) != "annotation_cze"]
   
      item_values2 <- item_values2[names(item_values2) != "annotation_eng"]
   
      item_names2 <- item_names2[item_names2 != "Anotace česky:"]
      items2 <- items2[items2 != "annotation_cze"]
   
      item_names2 <- item_names2[item_names2 != "Anotace anglicky:"]
      items2 <- items2[items2 != "annotation_eng"]
   }
   
   all_items <- list()
   
   for (i in seq_along(items2)) {
      
     
     all_items <- c(all_items, paste(item_names2[i], item_values2[i]))
     
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
 
 # Save extra values in state$values when we bookmark
 onBookmark(function(state) {
     state$values$section_iv_funded <- section_iv$funded
     state$values$section_iv_unfunded <- section_iv$unfunded
     
 })
 
 # Read values from state$values when we restore
 onRestore(function(state) {
     section_iv$funded <- state$values$section_iv_funded
     section_iv$unfunded <- state$values$section_iv_unfunded
 })  
  return(section_iv)
    
    
  })}
    
## To be copied in the UI
# mod_grants_ui("grants_ui_1")
    

 
