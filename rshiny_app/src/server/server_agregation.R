dt_agregation = reactiveValues(label_df_modele_choix = NULL, df_sim_terme_topic_modele = NULL, df_sim_doc_topic_modele = NULL, df_document_vector_modele = NULL,
                               df_informations_modele = NULL)

# udpate si on change de label pour un topic
observeEvent({
  input$Choix_modele_agreg
  input$boutons_label
  input$change_topic}, {
  dt_agregation$label_df_modele_choix = subset(dt_topics$label_df_choix, modele == input$Choix_modele_agreg)
}, priority = 2,ignoreNULL = FALSE,ignoreInit = FALSE)

observeEvent({input$Choix_modele_agreg
  input$change_topic}, {
  dt_agregation$df_sim_doc_topic_modele = subset(datas$df_sim_doc_topic, modele == input$Choix_modele_agreg)
  dt_agregation$df_sim_terme_topic_modele = subset(datas$df_sim_terme_topic, modele == input$Choix_modele_agreg)
  dt_agregation$df_document_vector_modele = subset(datas$df_document_vector, modele == input$Choix_modele_agreg)
  dt_agregation$df_informations_modele = subset(datas$df_informations, modele == input$Choix_modele_agreg)
  
},ignoreNULL = FALSE,ignoreInit = FALSE)

########################################################################################
print_informations = function(data_infos, data_docs){
  "Affiche les informations en HTML sur l'agrégation"
  
  nb_topic_before_agg = round(data_infos[which(data_infos$infos=='nb_topic_before_agg'),]$value)  #valeurs #nb_topic_avt_agreg
  message = paste("Nombre de topics avant agrégation :", nb_topic_before_agg)
  
  message = paste(message,'&emsp;&emsp;',"Nombre de topics après agrégation :", length(unique(data_docs$cluster)),'<br/>')
  return(HTML(message))
}

output$informations_metrique <- renderText({
  data_infos = dt_agregation$df_informations_modele
  data_docs = dt_agregation$df_document_vector_modele
  print_informations(data_infos, data_docs)
})

output$fct_perte <- renderUI({
  data_infos = dt_agregation$df_informations_modele
  score = as.character(round(data_infos[which(data_infos$infos=='loss_fct'),]$value,3))
  withMathJax(paste('$$Fonction~de~perte =  ... =',score,'$$'))
})

########################################################################################
melt_fct= function(matrice_sim,labels){
  melt_matrice_sim = cbind(matrice_sim, labels)
  names(melt_matrice_sim) = c(as.vector(labels),'cluster')
  melt_matrice_sim = melt(melt_matrice_sim)
  return(melt_matrice_sim)
}

plotly_sim_topics= function(label_df){
  "Affiche la matrice X associé au bon modèle (heatmap)"
  
  label_df = label_df[order(label_df$cluster),]
  
  matrice_sim = datas[[paste0('matrice_sim_topics_',input$Choix_modele_agreg)]]
  
  labels = label_df$label
  melt_matrice_sim = melt_fct(matrice_sim, labels)
  
  fig <- plot_ly(
    x = labels, y = labels,
    z = as.matrix(matrice_sim),
    colorscale='Viridis',
    reversescale =T, type = "heatmap", width = 1500, height = 1000
  )%>% 
    colorbar(title = 'Similarité')%>%
    add_annotations(x=melt_matrice_sim$cluster
                    ,y=melt_matrice_sim$variable
                    ,text=round(melt_matrice_sim$value,2)
                    ,showarrow=FALSE
                    ,align="center")%>%
    layout(yaxis=list(tickvals = labels)
           ,xaxis=list(tickvals = labels, tickangle = 45)
    )
  return(fig)
}

plot.sim_topics <- reactiveValues(out = NULL)

observe({
output$sim_topics_after <- renderPlotly({
  plotly_sim_topics(dt_agregation$label_df_modele_choix)
  })
}, priority = 1)