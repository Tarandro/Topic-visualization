tabItem(
  tabName = "topics",
    
  fluidRow(column(2,
            pickerInput("Choix_dataset", "Dataset", c('df_label','df_document_vector_before','df_document_vector','top_topic_terms','df_hierarchy_top_terms',
                                                      'df_sim_terme_topic', 'df_sim_doc_topic','df_informations'),selected = 'df_label')
          ),
          column(1),
          column(4,
                 checkboxGroupInput('box_models','Modèles :',as.vector(unique(df_label$modele)),as.vector(unique(df_label$modele)), inline = TRUE)
                 ),
          column(2,
                 actionButton("save_dataset", "Sauvegarder dataset")
                 )
  ),
    DT::dataTableOutput('dataset')

)