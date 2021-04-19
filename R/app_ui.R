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
      
   

        column(width = 7,
               
           
               navlistPanel(widths = c(4,8), well = F,
                            
                            "Researcher's details",
                            
                            tabPanel("IDENTIFIKAČNÍ ÚDAJE",
                                     
                                     mod_employee_name_ui("employee_name_ui_1"),
                                     
                                     mod_department_ui("department_ui_1"),
                                     
                                     mod_fte_ui("fte_ui_1"),
                                     textOutput("r$fte")
                                     
                                     ),
                            "Report sections",
                            tabPanel("I. VYDANÉ PUBLIKACE", 
                                     
                                     mod_pub_ui("pub_ui_1")
                                     ),
                            
                            tabPanel("II. ORGANIZACE KONFERENCÍ A WORKSHOPŮ", 
                                     
                                     h2("II. ORGANIZACE KONFERENCÍ A WORKSHOPŮ"),
                                     p( "Včetně odkazu do ASEP")
                                     
                                     ),
                            
                            tabPanel("III. PEDAGOGICKÁ A PŘEDNÁŠKOVÁ ČINNOST",
                                     
                                     h2("III. PEDAGOGICKÁ A PŘEDNÁŠKOVÁ ČINNOST"),
                                     
                                     p("Neuvádějte pedagogické aktivity nesouvisející s Vaším odborným působením ve FLU a vycházející např. z vedlejšího prac. poměru na VŠ."),
                                     
                                     tabsetPanel(tabPanel("Bc. & Mgr.",
                                     

                                     h3("1)	Výuka na vysokých školách a vedení prací:"),
                                     h4("a) Bakalářské a magisterské studijní programy"),
                                     mod_undergrad_ui("undergrad_ui_1"),
                                     ), 
                                     tabPanel("PhD.",
                                     h3("1)	Výuka na vysokých školách a vedení prací:"),
                                     h4("b) Doktorský studijní program"),
                                     mod_postgrad_ui("postgrad_ui_1"),
                                     ),
                                     
                                     tabPanel("Konference",
                                     h3("2)	Příspěvky a přednášky na konferencích: "),
                                     h4("a) Zahraniční:"),
                                     mod_conference_ui("conference_ui_1"),
                                     h4("b) Domácí:"),
                                     mod_conference_ui("conference_ui_2"), br(),br(),br(),br(),br(),    
                                     ),
                                     tabPanel("Přednášky",
                                     h3("3)	 Samostatné přednášky:"),
                                     h4("a) Zahraniční:"),
                                     h4("b) Domácí:")
                                     )
                                     )
                                     ),
                            
                            tabPanel("IV. ŘEŠENÉ ČI SPOLUŘEŠENÉ GRANTY",
                                     
                                     h2("IV. ŘEŠENÉ ČI SPOLUŘEŠENÉ GRANTY"),
                                     p("Uveďte i projekty podané a nepřijaté k financování.")
                                     
                                     
                                     ),
                            
                            
                            tabPanel("V. ŘEŠENÉ PROJEKTY V RÁMCI STRATEGIE AV 21",
                                     
                                     h2("V. ŘEŠENÉ PROJEKTY V RÁMCI STRATEGIE AV 21"),
                                     p("Včetně anotace (min. 300 znaků) a výstupů (publikace, konference, přednáška atd.).")
                                     
                                     
                                     ),
                            tabPanel("VI. POPULARIZAČNÍ ČINNOST"),
                            tabPanel("VII. SPOLUPRÁCE SE STÁTNÍ A VEŘEJNOU SPRÁVOU"),
                            tabPanel("VIII. ZAHRANIČNÍ SPOLUPRÁCE"),
                            tabPanel("IX. OSTATNÍ"),
                            tabPanel("X. ROZPRACOVANÉ PUBLIKACE A PROJEKTY"),
                            tabPanel("XI. RŮZNÉ")
                            
                        
               ),
        ),
        column(width = 5,
               
               h3("Download report"),
               
               mod_docx_ui("docx_ui_1"),
               
               
               mod_preview_ui("preview_ui_1"),
    
               
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

