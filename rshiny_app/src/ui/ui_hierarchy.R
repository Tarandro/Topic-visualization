tabItem(
  tabName = "topics",
  
  fluidRow(column(2,
                  pickerInput("Choix_modele_hier", "Modèle", as.vector(unique(top_topic_terms$modele)),selected = as.vector(unique(top_topic_terms$modele))[1])
  )),
  
  visNetworkOutput("network", width = "100%", height = "1000px")
  
)