tabItem(
  tabName = "agregation",
            fluidRow(column(6,fluidRow(
                      column(1),
                      column(5,
                   pickerInput("Choix_modele_agreg", "Modèle", as.vector(unique(top_topic_terms$modele)),selected = as.vector(unique(top_topic_terms$modele))[1])),
                      )
                    )),
            fluidRow(column(width=2),
                      column(
                        h4(p("Informations sur l'agrégation des thèmes",style="color:black;text-align:center")),
                        width=8,style="background-color:lavender;border-radius: 10px")),
             br(),
             fluidRow(tags$style(HTML("
                    div.MathJax_Display{
                    text-align: center !important;
                    }
                    #informations_metrique {
                    text-align: center;
                    }")),
                      withMathJax(),
                      column(width=2),align="center",
                      column(
                        p(htmlOutput("informations_metrique"),style="color:black"),
                        br(),
                        p("Lors de l'agrégation, toutes les paires de thèmes (matrice X) avec une similarité supérieure au Seuil_1 sont assemblées ensemble 
                          et les thèmes (matrice Y) avec une similarité entre documents inférieure au Seuil_2 sont assemblés avec le thème qui leur est le plus similaire. 
                          Ensuite, on obtient la fonction de perte suivante :",style="text-align:left"),
                        uiOutput("fct_perte",style="text-align:center;color:black;border:1px solid black;background-color:white"),
                        br(),
                        width=8,style="background-color:lavender;border-radius: 10px")),

               box(title = "X : Matrice de similarité entre thèmes",
                   width = 12,
                   height = 1100,
                   solidHeader = TRUE,
                   status = "primary",
                   plotlyOutput("sim_topics_after")
                    ),
            fluidRow(
                box(title = "Y : Matrice de similarité entre top termes des thèmes", width=6, height = 750,
                    solidHeader = TRUE, status = "primary",
                    plotlyOutput("sim_termes_after")
                ),
               box(title = "Matrice de similarité entre documents des thèmes", width=6,  height = 750,
                   solidHeader = TRUE, status = "primary",
                   plotlyOutput("sim_docs_after")
                   )
              
             )
  )
