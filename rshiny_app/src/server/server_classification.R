dt_classification = reactiveValues(df_document_vector_before_modele = NULL, df_document_vector_modele = NULL,
                                   df_document_vector_before_modele_subset = NULL, df_document_vector_modele_subset = NULL,
                                   label_df_modele_choix = NULL, df_info_classif_modele = NULL, myColors = NULL)  
  
# udpate si on change de label pour un topic
observeEvent({input$Choix_modele_clas
  input$boutons_label
  input$change_topic
  assembler()}, {
    dt_classification$label_df_modele_choix = subset(dt_topics$label_df_choix, modele == input$Choix_modele_clas)
    
    subset_label = dt_classification$label_df_modele_choix
    subset_label = subset_label[order(subset_label$label),]
    myColors = rainbow(length(subset_label$label))
    names(myColors) = subset_label$label
    dt_classification$myColors = myColors
    
},priority = 2,ignoreNULL = FALSE,ignoreInit = FALSE)  
  
  
  
observeEvent({input$Choix_modele_clas
  input$change_topic
  assembler()}, {
  df_document_vector_before_modele = subset(datas$df_document_vector_before, modele == input$Choix_modele_clas)
  dt_classification$df_document_vector_before_modele = df_document_vector_before_modele
  df_document_vector_modele = subset(datas$df_document_vector, modele == input$Choix_modele_clas)
  dt_classification$df_document_vector_modele = df_document_vector_modele
  
  dt_classification$df_document_vector_before_modele_subset = df_document_vector_before_modele[sample(1:length(df_document_vector_before_modele$terms)),][1:5000,]
  dt_classification$df_document_vector_modele_subset = df_document_vector_modele[sample(1:length(df_document_vector_modele$terms)),][1:5000,]
  
  dt_classification$df_info_classif_modele = subset(datas$df_info_classif, modele == input$Choix_modele_clas)

},priority = 1,ignoreNULL = FALSE,ignoreInit = FALSE)

########################################################################################

print_info_classif = function(data_infos){
  "Affiche les informations sur la classification"
  
  modele = data_infos$model[1]
  score_kfold = round(data_infos$score_kfold[1],3)
  message = paste('Modèle de classification : ',modele,'<br/>')
  message = paste(message,'Moyenne précision sur 5-validations  :',score_kfold)

  return(HTML(message))
}

output$info_classif <- renderUI({
  print_info_classif(dt_classification$df_info_classif_modele)
})
########################################################################################

plotly_tsne_docs_before = function(label_df, data_docs, myColors){
  "Affiche les documents (max 5000) des topics après réduction t-SNE en 2D  (avant la classification donc présence de bruit)"
  
  label_df = label_df[,c('label','cluster')]
  label_df$label = as.character(label_df$label)
  label_df = rbind(label_df,c('non labélisé',-1))
  label_df$label = as.factor(label_df$label)
  
  Colors = c()
  for (lab in label_df[order(label_df$label),]$label){
    if (is_in(lab,names(myColors))){
      Colors = c(Colors,myColors[lab])
    }
    else{
      color_non_label = "#000000"
      names(color_non_label) = 'non labélisé'
      Colors = c(Colors,color_non_label)
    }
  }
  
  data_docs$terms = as.character(data_docs$terms)
  for (row in 1:nrow(data_docs)){
    data_docs$terms[row] = fct_reduce_text(data_docs$terms[row],100)
  }
  
  data_docs = merge(data_docs,label_df[,c('label','cluster')])
  fig <- plot_ly(data = data_docs, x = ~tsne_1, y = ~tsne_2,color = ~label,
                 text = ~paste("Label :",label,'</br></br>Terme :', terms),
                 hoverinfo = 'text',colors=Colors,
                 type = 'scatter', mode = 'markers')%>%
    layout(title = 't-SNE représentation des documents (5000 max)',
           yaxis=list(title='axis 2',showgrid=FALSE,zeroline=FALSE,tickvals=c(''))
           ,xaxis=list(title='axis 1',showgrid=FALSE,zeroline=FALSE,tickvals=c('')) )
  return(fig)
}

plot.tsne_docs_before <- reactiveValues(out = NULL)

observe({
  output$tsne_docs_before <- renderPlotly({
    plotly_tsne_docs_before(dt_classification$label_df_modele_choix, dt_classification$df_document_vector_before_modele_subset, dt_classification$myColors)
    })
}, priority = 0)

########################################################################################

plotly_tsne_docs_after = function(label_df, data_docs, myColors){
  "Affiche les documents (max 5000) des topics après réduction t-SNE en 2D  (après la classification)"
  
  data_docs$terms = as.character(data_docs$terms)
  for (row in 1:nrow(data_docs)){
    data_docs$terms[row] = fct_reduce_text(data_docs$terms[row],100)
  }
  
  data_docs = merge(data_docs,label_df[,c('label','cluster')])
  fig <- plot_ly(data = data_docs, x = ~tsne_1, y = ~tsne_2,color = ~label,
                 text = ~paste("Label :",label,'</br></br>Terme :', terms),
                 hoverinfo = 'text',colors=myColors,
                 type = 'scatter', mode = 'markers')%>%
    layout(title = 't-SNE représentation des documents (5000 max)',
           yaxis=list(title='axis 2',showgrid=FALSE,zeroline=FALSE,tickvals=c(''))
           ,xaxis=list(title='axis 1',showgrid=FALSE,zeroline=FALSE,tickvals=c('')) )
  return(fig)
}

plot.tsne_docs_after <- reactiveValues(out = NULL)

observe({
  output$tsne_docs_after <- renderPlotly({
    plotly_tsne_docs_after(dt_classification$label_df_modele_choix, dt_classification$df_document_vector_modele_subset, dt_classification$myColors)
    })
}, priority = 0)

########################################################################################

plotly_pie_docs_before= function(label_df, data_docs, myColors){
  "Affiche le nombre de documents pour chaque topic sous forme camembert (avant la classification donc présence de bruit)"
  
  label_df = label_df[,c('label','cluster')]
  label_df$label = as.character(label_df$label)
  label_df = rbind(label_df,c('non labélisé',-1))
  label_df$label = as.factor(label_df$label)
  
  data_docs = merge(data_docs,label_df[,c('label','cluster')])
  data_docs = sqldf("SELECT label,COUNT(modele) as count FROM data_docs GROUP BY  label;")
  data_docs = data_docs[order(-data_docs$count),]
  
  Colors = c()
  for (lab in data_docs$label){
    if (is_in(lab,names(myColors))){
      Colors = c(Colors,myColors[lab])
    }
    else{
      Colors = c(Colors,"#000000")
    }
  }
  
  fig <- plot_ly(data = data_docs, labels = ~label, values = ~count,
                 type = 'pie')%>%
    layout(colorway = Colors, legend = list(orientation = 'h') )
  return(fig)
}

plot.pie_docs_before <- reactiveValues(out = NULL)

observe({
  output$pie_docs_before <- renderPlotly({
    plotly_pie_docs_before(dt_classification$label_df_modele_choix, dt_classification$df_document_vector_before_modele, dt_classification$myColors)
    })
}, priority = 0)

########################################################################################

plotly_pie_docs_after = function(label_df, data_docs, myColors){
  "Affiche le nombre de documents pour chaque topic sous forme camembert (après la classification)"
  
  data_docs = merge(data_docs,label_df[,c('label','cluster')])
  data_docs = sqldf("SELECT label,COUNT(modele) as count FROM data_docs GROUP BY  label;")
  data_docs = data_docs[order(-data_docs$count),]
  
  Colors = c()
  for (lab in data_docs$label){
    Colors = c(Colors,myColors[lab])
  }
  
  fig <- plot_ly(data = data_docs, labels = ~label, values = ~count,
                 type = 'pie')%>%
    layout(colorway = Colors, legend = list(orientation = 'h') )
  return(fig)
}

plot.pie_docs_after <- reactiveValues(out = NULL)

observe({
  output$pie_docs_after <- renderPlotly({
    plotly_pie_docs_after(dt_classification$label_df_modele_choix, dt_classification$df_document_vector_modele, dt_classification$myColors)
    })
}, priority = 0)
