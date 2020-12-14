dt_visualisation = reactiveValues(df_document_vector_modele = NULL, df_document_vector_modele_subset = NULL, label_df_modele_choix = NULL,
                                  df_document_vector_modele_date = NULL, top_topic_terms_modele_cluster = NULL,myColors = NULL)

# udpate si on change de label pour un topic
observeEvent({input$Choix_modele_viz
  input$boutons_label
  input$change_topic}, {
  dt_visualisation$label_df_modele_choix = subset(dt_topics$label_df_choix, modele == input$Choix_modele_viz)
  updatePickerInput(session, inputId ="Choix_themes_viz", choices = as.vector(dt_visualisation$label_df_modele_choix$label),
                    selected = as.vector(dt_visualisation$label_df_modele_choix$label))
  },priority = 2,ignoreNULL = FALSE,ignoreInit = FALSE)

observeEvent({input$Choix_themes_viz
  input$Choix_modele_viz
  input$change_topic}, {
    
  dt_visualisation$df_label_modele_cluster = subset(dt_visualisation$label_df_modele_choix, label %in% input$Choix_themes_viz)
  subset_label = dt_visualisation$df_label_modele_cluster 
  dt_visualisation$top_topic_terms_modele_cluster = subset(datas$top_topic_terms, modele == input$Choix_modele_viz &
           cluster %in% subset_label[which(subset_label$modele==input$Choix_modele_viz),]$cluster)
  df_document_vector_modele = subset(datas$df_document_vector, modele == input$Choix_modele_viz &
                                       cluster %in% subset_label[which(subset_label$modele==input$Choix_modele_viz),]$cluster)
  dt_visualisation$df_document_vector_modele = df_document_vector_modele
  dt_visualisation$df_document_vector_modele_subset = df_document_vector_modele[sample(1:length(df_document_vector_modele$terms)),][1:5000,]
  dt_visualisation$df_document_vector_modele_date = subset(df_document_vector_modele, date >= input$dateRange[1] & date <= input$dateRange[2])
  
  dt_visualisation$df_label_modele_cluster = subset_label[order(subset_label$label),]
  if (length(subset_label$label) > 0){
  myColors = rainbow(length(subset_label$label))
  names(myColors) = dt_visualisation$df_label_modele_cluster$label
  dt_visualisation$myColors = myColors
  }
},priority = 1,ignoreNULL = FALSE,ignoreInit = FALSE)

observeEvent(input$dateRange, {
  dt_visualisation$df_document_vector_modele_date = subset(dt_visualisation$df_document_vector_modele,
                                                           date >= input$dateRange[1] & date <= input$dateRange[2])
},ignoreNULL = FALSE,ignoreInit = FALSE)

########################################################################################

plotly_tsne_word= function(label_df, top_terms, myColors){
  "Affiche les tops termes des topics après réduction t-SNE en 2D"
  
  top_terms = merge(top_terms,label_df[,c('label','cluster')])
  fig <- plot_ly(data = top_terms, x = ~tsne_1, y = ~tsne_2,color = ~label,
                 text = ~paste("Label :",label,'</br></br>Terme :', terms),size = ~freq,
                 hoverinfo = 'text',colors=myColors,
                 type = 'scatter', mode = 'markers')%>%
    layout(title = 't-SNE représentation des top termes',
           yaxis=list(title='axis 2',showgrid=FALSE,zeroline=FALSE,tickvals=c(''))
           ,xaxis=list(title='axis 1',showgrid=FALSE,zeroline=FALSE,tickvals=c('')))
  return(fig)
}

plot.tsne_word <- reactiveValues(out = NULL)

observe({
  output$tsne_word <- renderPlotly({
    plotly_tsne_word(dt_visualisation$df_label_modele_cluster, dt_visualisation$top_topic_terms_modele_cluster, dt_visualisation$myColors)
    })
}, priority = 0)

########################################################################################

fct_reduce_text = function(texte,longueur){
  texte = as.character(texte)
  if (is.na(texte)){
    return(texte)
  }
  else if (nchar(texte) <= longueur){
    return(texte)
  }
  else{
    return(paste(substr(texte, 1, longueur),"..."))
  }
}

plotly_tsne_docs = function(label_df, data_docs, myColors){
  "Affiche les documents (max 5000) des topics après réduction t-SNE en 2D"
  
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
           ,xaxis=list(title='axis 1',showgrid=FALSE,zeroline=FALSE,tickvals=c('')))
  return(fig)
}

plot.tsne_docs <- reactiveValues(out = NULL)

observe({
  output$tsne_docs <- renderPlotly({
    plotly_tsne_docs(dt_visualisation$df_label_modele_cluster, dt_visualisation$df_document_vector_modele_subset, dt_visualisation$myColors)
    })
}, priority = 0)

########################################################################################

plotly_bar_docs = function(label_df, data_docs, myColors){
  "Nombre de documents par topic (représentation bar)"
  
  data_docs = merge(data_docs,label_df[,c('label','cluster')])
  data_docs = sqldf("SELECT label, COUNT(modele) as count FROM data_docs GROUP BY  label;")
  data_docs = data_docs[order(data_docs$count),]
  
  fig <- plot_ly(x = reorder(as.character(data_docs$label),rev(data_docs$count)),y = data_docs$count,
                 color = reorder(as.character(data_docs$label),rev(data_docs$count)),colors=myColors,
                 type = 'bar')%>%
    layout(title = 'Nombre de documents par thèmes', showlegend = FALSE)
  return(fig)
}

plot.bar_docs<- reactiveValues(out = NULL)

observe({
  output$bar_docs <- renderPlotly({
    plotly_bar_docs(dt_visualisation$df_label_modele_cluster, dt_visualisation$df_document_vector_modele, dt_visualisation$myColors)
    })
}, priority = 0)

########################################################################################

plotly_line_graph = function(label_df, data_docs, myColors){
  "Séries temporelles : nombre de documents par date et topic"
  
  if (is_in('date',names(data_docs))){
    data_docs$date = as.Date(data_docs$date, "%d/%m/%Y")
  }
  data_docs = merge(data_docs,label_df[,c('label','cluster')])
  
  label_count = sqldf("SELECT label,COUNT(modele) as count FROM data_docs GROUP BY  label;")
  labels = as.vector(label_count[order(-label_count$count),]$label[1:min(length(label_count$label),8)])
  data_docs = data_docs[which(data_docs$label %in% labels),]
  
  Colors = c()
  for (i in 1:length(myColors)){
    if (is_in(names(myColors)[i],labels)){
      Colors = c(Colors,myColors[i])
    }
  }
  
  data_docs = sqldf("SELECT date,label,COUNT(modele) as count FROM data_docs GROUP BY  date,label;")
  fig <- plot_ly(data_docs, x = ~date, y = ~count,color = ~label,colors=Colors,
                 type = 'scatter', mode = 'lines')%>%
    layout(title = 'Nombre de documents par thèmes (max 8) et date')
  return(fig)
}

plot.line_graph <- reactiveValues(out = NULL)

observe({
  output$line_graph <- renderPlotly({
    req(length(unique(dt_visualisation$df_document_vector_modele_date$date))>1)
    plotly_line_graph(dt_visualisation$df_label_modele_cluster, dt_visualisation$df_document_vector_modele_date, dt_visualisation$myColors)
      })
}, priority = 0)