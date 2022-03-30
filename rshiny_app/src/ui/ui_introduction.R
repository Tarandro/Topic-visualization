tabItem(
  tabName = "introduction",
  
  fluidRow(column(width=2),
           column(
             h3(p("Extraction et labélisation automatique de topics",style="color:black;text-align:center")),
             width=8,style="background-color:lavender;border-radius: 10px")),
  
  br(),
  
  fluidRow(column(width=2),
           column(
             h4(p("Contexte Métier :",style="color:black")),
             br(),
             p("L'application s'apparente comme une analyse exploratoire des données textuelles. 
               Un utilisateur peut découvrir et explorer un corpus de documents à travers cette application. Il peut en un coup d’œil 
               avoir une liste des sujets évoqués et un potentiel titre générique à chacun de ses sujets. Pour l’instant, l’application est 
               centrée sur la comparaison des méthodes, avec des sorties qui diffèrent en fonction du modèle sélectionné pour 
               le topic modeling et la labélisation mais on pourrait imaginer à terme lorsque la méthodologie sera plus aboutie 
               une application qui fera un choix automatique sur le meilleur modèle de topic modeling et de labélisation.",style="text-align:left"),
             br(),
             p("L’extraction des topics est utilisée pour découvrir la structure sémantique d’une collection de documents. 
               Plusieurs méthodes d’extraction de topics existent mais l’environnement textuel sans labels et la variété 
               des styles textuels impliquent une difficulté dans l’automatisation et l’évaluation des extractions de topics. 
               L'application permet de visualiser les différentes méthodes et d'aider l'utilisateur a comprendre ses topics.",style="text-align:left"),
             br(),
             p("La labélisation de topics est une étape permettant de valoriser les topics en leur donnant un titre indicatif.",style="text-align:left"),
             width=8,style="background-color:lavender;border-radius: 10px")),
  
  br(),
  
  fluidRow(column(width=2),
           column(
             h4(p("Contexte Technique :",style="color:black")),
             br(),
             p("Le projet consiste à extraire et labéliser automatiquement des topics. 
             L’application exploite différents modèles d’extraction de topics et la comparaison entre ces méthodes. 
             Nous présentons ensuite une nouvelle méthode qui allie agrégation de topics, recherche des meilleurs paramètres 
             et évaluation des modèles le tout dirigé par une mesure de performance. Cette méthode permet d’automatiser 
             le processus d’extraction de topics et de posséder une mesure de performance",style="text-align:left"),
             width=8,style="background-color:lavender;border-radius: 10px")),
  
  br(),
  
  fluidRow(column(width=2),
           column(
             
             h4(p("Jeu de données :",style="color:black")),
             br(),
             p("
             CDP is a global non-profit that drives companies and governments to reduce their greenhouse gas emissions, 
             safeguard water resources, and protect forests. Each year, CDP takes the information supplied in its 
             annual reporting process and scores companies and cities.
             The CDP dataset consists of publicly available responses to 3 different surveys: 
               (1) corporate climate change disclosures; 
               (2) corporate water security disclosures; 
               and (3) disclosures from cities. Data is available for 2018, 2019, and 2020",style="text-align:left"),

             fluidRow(
               column(width=10),
               column(width=2,
                      actionButton("bouton_other_examples", "Autres exemples", width = "100%")
                      )
             ),
             
             DT::DTOutput('example_dataset'),
             
             width=8,style="background-color:lavender;border-radius: 10px")),
)