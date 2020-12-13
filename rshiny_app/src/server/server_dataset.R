observeEvent(input$box_models, {
  subset_name_dataset = name_dataset
  for(modele in input$box_models){
    subset_name_dataset = c(subset_name_dataset,paste0('matrice_sim_topics_',modele))
  }
  updatePickerInput(session = session, inputId = "Choix_dataset", "Dataset",subset_name_dataset,selected = 'df_label')
})

to_factor = function(name_columns){
  col_to_factor = c()
  for (col in name_columns) {
    if (col %in% c('cluster','modele','choix')){
      col_to_factor = c(col_to_factor,col)
    }
  }
  return(col_to_factor)
}

# affiche le dataset sélectionné
observeEvent(input$Choix_dataset, {
  
  if (input$Choix_dataset %in% c('df_document_vector_before','df_document_vector','top_topic_terms','df_sim_terme_topic','df_sim_doc_topic','df_label')){
    output$dataset = renderDataTable({
      data_to_show = datas[[input$Choix_dataset]]
      data_to_show = subset(data_to_show, modele %in% input$box_models)
      col_to_factor = to_factor(names(data_to_show))
      data_to_show[col_to_factor] <- lapply(data_to_show[col_to_factor], as.factor)
      data_to_show[sapply(data_to_show, is.numeric)] <- lapply(data_to_show[sapply(data_to_show, is.numeric)], round, 2)
      data_to_show
  }, filter = "top",
  options = list(pageLength = 20)
  )
  }
  else{
    output$dataset = renderDataTable({
      data_to_show = datas[[input$Choix_dataset]]
      if (input$Choix_dataset %in% c('df_word_cluster_tf','df_word_cluster_tfidf')) {
        data_to_show = subset(data_to_show, modele %in% input$box_models)
      }
      data_to_show[sapply(data_to_show, is.numeric)] <- lapply(data_to_show[sapply(data_to_show, is.numeric)], round, 2)
      data_to_show
    },
    options = list(pageLength = 20, searching = FALSE,escape = FALSE)
    )
  }
})

# sauvegarde tous les subset(dataset, modele %in% input$box_models)
observeEvent(input$save_dataset, {
  subset_name_dataset = name_dataset
  for(modele in input$box_models){
    subset_name_dataset = c(subset_name_dataset,paste0('matrice_sim_topics_',modele))
  }

  withProgress(message = 'Sauvegarde', value = 0, {
    n <- length(subset_name_dataset)
          for (name in subset_name_dataset) {
            write.csv(subset(datas[[name]], modele %in% input$box_models),paste0(path,name,'_new.csv'), row.names=FALSE)
            incProgress(1/n)
          }
  })
})