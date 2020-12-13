tabItem(
  tabName = "visualisation",
    fluidRow(column(3,
                 pickerInput("Choix_modele_clas", "Modèle", as.vector(unique(top_topic_terms$modele)),selected = as.vector(unique(top_topic_terms$modele))[1])
                 ),
             column(2),
             column(4,
                 h4(p(htmlOutput("info_classif"),style="color:black")),
                 style="background-color:lavender;border-radius: 10px"),
       ),
      
      box(title = "T-SNE représentations des documents",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          h3('Avant la classification : '),
          plotlyOutput("tsne_docs_before"),
          h3('Après la classification : '),
          plotlyOutput("tsne_docs_after"),
      ), 
      box(title = "Nombre de documents par thèmes", width=12, 
          solidHeader = TRUE, status = "primary",
          fluidRow(
            column(6,
          h3('Avant la classification : '),
          plotlyOutput("pie_docs_before")
            ),
          column(6,
          h3('Après la classification : '),
          plotlyOutput("pie_docs_after")
          )
          )
      )
      

  )
