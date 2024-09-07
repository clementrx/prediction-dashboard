ui <- fluidPage(
  useShinyjs(),
  navbarPage(
    theme = shinytheme('cerulean'),
    
    "PMU",
    
    tabPanel("Upload CSV",
             fluidRow(
               column(4, fileInput("file", "Choisir un fichier CSV", accept = c(".csv")))
             ),
             fluidRow(
               column(12, tableOutput("file_contents"))
             )),
    
    
    tabPanel("Graphiques",
             fluidRow(
               div(id = "Sidebar", sidebarPanel(width = 12,
                                                fluidRow(
                                                  column(4, uiOutput('hipp_id_graph')),
                                                  column(6, uiOutput('course_filter_ui_graph'))),
                                                fluidRow(textOutput("heure")))
               ),
               mainPanel(width = "100%",
                         fluidRow(
                           column(4, actionButton("hideSidebar", "Cacher/afficher les filtres", class = "btn btn-sm"))
                         ),
                         # Boîtes au-dessus du tableau
                         fluidRow(
                           column(4, uiOutput("sg_box")),
                           column(4, uiOutput("sp_box")),
                           column(4, uiOutput("multi_box"))
                         ),
                         fluidRow(gt_output("mytable")))))
    
    
  )
)