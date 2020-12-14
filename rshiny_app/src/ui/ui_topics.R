tabItem(
  tabName = "topics",
  tabsetPanel(
    id = "tabs_topics",
    tabPanel("Modèles",
             br(),
             fluidRow(column(width=2),
                      column(
                        h4(p("Modèles et Résultats des fonctions de perte",style="color:black;text-align:center")),
                        width=8,style="background-color:lavender;border-radius: 10px")),
             br(),
              fluidRow(column(width=2),align="center",
                       column(8,
                              br(),
                              DT::dataTableOutput('methodes'),
                              br(),
                              style="background-color:lavender;border-radius: 10px"
                       )
              ),
             DTOutput('table_labels')
    ),
    
    tabPanel("Thèmes", 
             fluidRow(
               column(3,
                      pickerInput("Choix_modele", "Modèle", as.vector(unique(top_topic_terms$modele)),selected = as.vector(unique(top_topic_terms$modele))[1])
               ),
               column(2),
               column(4,
                      h4(p(textOutput("nombre_documents"),style="color:black")),
                      h4(p(textOutput("nombre_themes"),style="color:black")),
                      style="background-color:lavender;border-radius: 10px"),
               ),
             fluidRow(
               tags$script("Shiny.addCustomMessageHandler('resetInputValue', function(variableName){
                   Shiny.onInputChange(variableName, null);
                   });
                   "),
               box(title = "Thèmes et top termes",
                   width = 12,
                   solidHeader = TRUE,
                   status = "primary", 
                   tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: purple}")),
                   fluidRow(
                     column(3,
                   pickerInput("choix_classement", "Classement", c("Nombre de documents","Moyenne des similarités entre documents"),selected = "Nombre de documents")
                     ),
                     column(1),
                     column(5,
                   sliderInput("select_nb_topterms", label = "Nombre de top termes", min = 1, 
                               max = 30, value = 10)
                     ),
                   ),
                   #h3(uiOutput("print_topics"))
                   DT::dataTableOutput('table_topics')
               ),
               box(title = "Repartition des thèmes", width=12, 
                   solidHeader = TRUE, status = "primary", 
                   plotlyOutput("repartition_topics")))
             ), 
    tabPanel("Documents", 
             fluidRow(
               column(3,
               pickerInput("Choix_themes", "Thème", as.vector(df_label[which(df_label$modele==as.vector(unique(top_topic_terms$modele))[1] & df_label$choix==1),]$label),
                           selected = as.vector(df_label[which(df_label$modele==as.vector(unique(top_topic_terms$modele))[1] & df_label$choix==1),]$label[1]), 
                           options = list(`actions-box` = TRUE))
               ),
               column(1),
               column(3,
               radioButtons("boutons_label", label = "Choix du label",
                            choices = as.vector(df_label[which(df_label$modele==as.vector(unique(top_topic_terms$modele))[1] & df_label$cluster==unique(df_label$cluster)[1]),]$label), 
                            selected = as.vector(df_label[which(df_label$modele==as.vector(unique(top_topic_terms$modele))[1] & df_label$cluster==unique(df_label$cluster)[1] & df_label$choix==1),]$label[1]))
               ),
               column(1),
               column(3, 
                      textInput("autre_label", "Autre label :", 
                                  value = "")
               )
             ),
              
             fluidRow(
               column(4,
                      h3(htmlOutput("top_termes"))
             ), 
             column(8,
                    plotlyOutput("freq_top_termes")
             )
             ),
             
             h3(htmlOutput("exemple_docs")),
             
             fluidRow(
               tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: purple}")),
               column(2,
                      actionButton("bouton_rerun", "Autres exemples")
               ),
               column(2),
               column(6,
                      sliderInput("select_lim_top_termes", label = "Limite du nombre de top termes à surligner", min = 1, 
                                  max = 100, value = 50)       
               )
             ),
             
             actionButton("bouton_retour", "Retour liste des thèmes")
    ),
    
    tabPanel("NMF_topic", 
             fluidRow(
               column(3,
                      pickerInput("Choix_themes_nmf", "Thème", as.vector(df_label[which(df_label$modele==as.vector(unique(top_topic_terms$modele))[1] & df_label$choix==1),]$label),
                                  selected = as.vector(df_label[which(df_label$modele==as.vector(unique(top_topic_terms$modele))[1] & df_label$choix==1),]$label[1]), 
                                  options = list(`actions-box` = TRUE))
               ),
               column(3, 
                      numericInput("k_topics", 
                                   h4("Nombre de topics :"), 
                                   value = 10)
               )
             ),
             fluidRow(
               column(1),
               column(2,
                      actionButton("nmf", "Lancer NMF")
               ),
               column(3),
               column(2,
                      actionButton("change_topic", "Changement topics")
               )
             ),
             br(),
             fluidRow(
               column(6,
                      h4(p(textOutput("info_nmf"),style="color:black"))
                ),
             ),
             box(title = "Thèmes et top termes",
                 width = 12,
                 solidHeader = TRUE,
                 status = "primary", 
                 tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: purple}")),
              fluidRow(
                   tags$head(
                     tags$script('
                        Shiny.addCustomMessageHandler("unbinding_table_elements", function(x) {                
                        Shiny.unbindAll($(document.getElementById(x)).find(".dataTable"));
                        });')
                     ) ),
             DT::dataTableOutput('table_topics_nmf')
             )
    )
  )
  )
    