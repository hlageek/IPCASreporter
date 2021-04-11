#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    fluidPage(
      titlePanel("Submission form for IP CAS annual report"),
      
   

        column(width = 10,
               
                      
                      
                      
           
               navlistPanel(widths = c(2,8), well = F,
                            
                            tabPanel("Identifkační údaje",
                                     mod_employee_name_ui("employee_name_ui_1"),
                                     
                                     mod_department_ui("department_ui_1"),
                                     
                                     mod_fte_ui("fte_ui_1"),
                                     textOutput("fte")
                                     
                                     ),
                            "Report sections",
                            tabPanel("I. VYDANÉ PUBLIKACE", 
                                     
                                     mod_pub_ui("pub_ui_1")
                                     ),
                            tabPanel("II. ORGANIZACE KONFERENCÍ A WORKSHOPŮ", h2("2")),
                            tabPanel("III. PEDAGOGICKÁ A PŘEDNÁŠKOVÁ ČINNOST"),
                            tabPanel("IV. ŘEŠENÉ ČI SPOLUŘEŠENÉ GRANTY"),
                            tabPanel("V. ŘEŠENÉ PROJEKTY V RÁMCI STRATEGIE AV 21"),
                            tabPanel("VI. POPULARIZAČNÍ ČINNOST"),
                            tabPanel("VII. SPOLUPRÁCE SE STÁTNÍ A VEŘEJNOU SPRÁVOU"),
                            tabPanel("VIII. ZAHRANIČNÍ SPOLUPRÁCE"),
                            tabPanel("IX. OSTATNÍ"),
                            tabPanel("X. ROZPRACOVANÉ PUBLIKACE A PROJEKTY"),
                            tabPanel("XI. RŮZNÉ")
                            
                        
               ),
        ),
        column(width = 2,
               
               h1("test"),
               
               mod_docx_ui("docx_ui_1")
               
               
               
               
        )
        
        
    ),
    
  )
  
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'IPCASreporter'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

