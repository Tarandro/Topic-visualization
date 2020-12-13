tabItem(
  tabName = "topics",
    
  fluidRow(column(2,
            pickerInput("Choix_dataset", "Dataset", c(name_dataset,name_modeles),selected = 'df_label')
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