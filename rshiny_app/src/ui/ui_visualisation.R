tabItem(
  tabName = "visualisation",
    fluidRow(
      column(3,
      pickerInput("Choix_modele_viz", "Modèle", as.vector(unique(top_topic_terms$modele)),selected = as.vector(unique(top_topic_terms$modele))[1]),
      ),
      column(2),
      column(3,
      pickerInput("Choix_themes_viz", "Thèmes", as.vector(df_label[which(df_label$modele==as.vector(unique(top_topic_terms$modele))[1] & df_label$choix==1),]$label),
                  selected = as.vector(df_label[which(df_label$modele==as.vector(unique(top_topic_terms$modele))[1] & df_label$choix==1),]$label), 
                  options = list(`actions-box` = TRUE), multiple = T),
      ),
    ),
      box(title = "T-SNE représentations",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          plotlyOutput("tsne_word"),
          plotlyOutput("tsne_docs")
      ), 
      box(title = "Nombre de documents", width=12, 
          solidHeader = TRUE, status = "primary",
          plotlyOutput("bar_docs"),
          dateRangeInput('dateRange',
                         label = 'Plage de date:',
                         start = min(df_document_vector$date), end = max(df_document_vector$date),
                         min = min(df_document_vector$date), max = max(df_document_vector$date),startview = 'year', language = 'fr', weekstart = 1
          ),
          plotlyOutput("line_graph")
      )
  )