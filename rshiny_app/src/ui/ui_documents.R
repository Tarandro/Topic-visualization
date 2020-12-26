tabItem(
  tabName = "topics",
  
  fluidRow(
    tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: purple}")),
    column(2,
                  pickerInput("Choix_modele_doc", "Modèle", as.vector(unique(top_topic_terms$modele)),selected = as.vector(unique(top_topic_terms$modele))[1])
          ),
    column(2),
    column(4,
           sliderInput("select_lim_top_termes_doc", label = "Limite du nombre de top termes à surligner", min = 1, 
                       max = 100, value = 100)       
    ),
    column(2,
           actionButton("bouton_rerun_doc", "Autres exemples", style='height:60px')
           , style = "margin-top: 30px;"
    ),
  ),
  
  h3(htmlOutput("example_documents"))
  
)