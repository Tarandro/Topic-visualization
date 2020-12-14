session$onSessionEnded(stopApp)


dt_topics = reactiveValues(df_document_vector_modele = NULL, top_topic_terms_modele = NULL, df_sim_doc_topic_modele = NULL,
                           label_df_modele_choix = NULL,df_document_vector_modele_cluster = NULL, top_topic_terms_modele_cluster = NULL,
                           df_label_modele_cluster = NULL,df_word_cluster_tf_modele_cluster = NULL, df_word_cluster_tfidf_modele_cluster= NULL,
                           df_label_modele_cluster_all_choix = NULL, label_df_choix = NULL, length_nmf = NULL, top_termes_nmf = NULL,
                           doc_topic = NULL, yolo = 0, tuples = NULL)

#assembler <- reactive({
#  paste(input$assembler_1,input$assembler_2,input$assembler_3,input$assembler_4)
#})

observeEvent(input$Choix_themes, {
  
  subset_label = dt_topics$label_df_modele_choix
  CLUSTER = subset_label[which(subset_label$label %in% input$Choix_themes & subset_label$modele==input$Choix_modele),]$cluster
  
  dt_topics$top_topic_terms_modele_cluster = subset(datas$top_topic_terms, modele == input$Choix_modele & cluster %in% CLUSTER)
  dt_topics$df_label_modele_cluster = subset(subset_label, cluster %in% CLUSTER)
  
  df_word_cluster_tf_modele= subset(datas$df_word_cluster_tf, modele == input$Choix_modele)
  df_word_cluster_tf_modele_cluster = df_word_cluster_tf_modele[,c('terms',paste0('X',CLUSTER))]
  names(df_word_cluster_tf_modele_cluster) = c('terms','freq')
  dt_topics$df_word_cluster_tf_modele_cluster = df_word_cluster_tf_modele_cluster

  df_word_cluster_tfidf_modele= subset(datas$df_word_cluster_tfidf, modele == input$Choix_modele)
  df_word_cluster_tfidf_modele_cluster = df_word_cluster_tfidf_modele[,c('terms',paste0('X',CLUSTER))]
  names(df_word_cluster_tfidf_modele_cluster) = c('terms','freq')
  dt_topics$df_word_cluster_tfidf_modele_cluster = df_word_cluster_tfidf_modele_cluster
  
  dt_topics$df_label_modele_cluster_all_choix = subset(datas$df_label, modele == input$Choix_modele & cluster %in% CLUSTER)
  data = dt_topics$df_label_modele_cluster_all_choix
  updateRadioButtons(session, inputId ="boutons_label",
                     choices = as.vector(data$label),
                     selected = as.vector(data[which(data$choix==1),]$label[1]))
}, priority = 1,ignoreNULL = FALSE,ignoreInit = FALSE)

# sélectionne le bouton rerun, sample les données appartenant au topic
observeEvent({input$bouton_rerun
  input$Choix_themes},{
  df_document_vector_modele_cluster = subset(datas$df_document_vector, modele == input$Choix_modele)
  dt_topics$df_document_vector_modele_cluster = df_document_vector_modele_cluster[sample(1:length(df_document_vector_modele_cluster$terms)),]
},ignoreNULL = FALSE,ignoreInit = FALSE)

# Si on écrit dans la case 'autre label' alors un bouton 'autre label' se rajoute dans la liste des labels du topic
observeEvent(input$autre_label, {
  if (input$autre_label != ""){
  data = dt_topics$df_label_modele_cluster_all_choix
  updateRadioButtons(session, inputId ="boutons_label",
                     choices = c(as.vector(data$label),'autre label'),
                     selected = as.vector(data[which(data$choix==1),]$label[1]))
  }
},ignoreNULL = FALSE,ignoreInit = FALSE)

# Si on change de label, changer le label dans les subset
observeEvent(input$boutons_label, {
  
  df_label_1 = datas$df_label
  df_label_1[which(df_label_1$modele == input$Choix_modele & df_label_1$cluster %in% df_label_1[which(df_label_1$model ==input$Choix_modele & df_label_1$label == input$Choix_themes),]$cluster),'choix'] = 0
  if (input$boutons_label != 'autre label'){
    df_label_1[which(df_label_1$modele == input$Choix_modele & df_label_1$cluster %in% df_label_1[which(df_label_1$model ==input$Choix_modele & df_label_1$label == input$Choix_themes),]$cluster & df_label_1$label==input$boutons_label),'choix'] = 1
    updatePickerInput(session = session, inputId = "Choix_themes", 
                      choices = as.vector(df_label_1[which(df_label_1$modele==input$Choix_modele  & df_label_1$choix==1),]$label),
                      selected = as.vector(input$boutons_label))
  }
  else{
    if (input$autre_label %in% df_label_1[which(df_label_1$modele == input$Choix_modele & df_label_1$cluster %in% df_label_1[which(df_label_1$model ==input$Choix_modele & df_label_1$label == input$Choix_themes),]$cluster),]$label){
      df_label_1[which(df_label_1$modele == input$Choix_modele & df_label_1$cluster %in% df_label_1[which(df_label_1$model ==input$Choix_modele & df_label_1$label == input$Choix_themes),]$cluster & df_label_1$label==input$boutons_label),'choix'] = 1
    }
    else{
      df_label_1$label = as.character(df_label_1$label)
      df_label_1 = rbind(df_label_1,c(input$autre_label,df_label_1[which(df_label_1$model ==input$Choix_modele & df_label_1$label == input$Choix_themes),]$cluster, 1, input$Choix_modele))
      df_label_1$label = as.factor(df_label_1$label)
    }
    updatePickerInput(session = session, inputId = "Choix_themes", 
                      choices = as.vector(df_label_1[which(df_label_1$modele==input$Choix_modele  & df_label_1$choix==1),]$label),
                      selected = as.vector(input$autre_label))
    updateTextInput(session,"autre_label", value = "")
  }
  
  datas$df_label = df_label_1
  
  dt_topics$label_df_modele_choix = subset(df_label_1, modele == input$Choix_modele & choix == 1)
  dt_topics$label_df_choix = subset(df_label_1, choix == 1)
  
}, priority = 2,ignoreNULL = FALSE,ignoreInit = FALSE)

observeEvent({input$Choix_modele
  input$change_topic},{

  df_label_1 = subset(datas$df_label, modele == input$Choix_modele)

  df_label_3 = subset(df_label_1, choix == 1 )
  updatePickerInput(session = session, inputId = "Choix_themes", 
                    choices = as.vector(df_label_3$label),
                    selected = as.vector(unique(df_label_3$label)[1]))
  
  
  data = subset(df_label_1, cluster %in% df_label_1[which(df_label_1$label == as.vector(unique(df_label_3$label)[1])),]$cluster)
  dt_topics$df_label_modele_cluster_all_choix = data
  updateRadioButtons(session, inputId ="boutons_label",
                     choices = as.vector(data$label),
                     selected = as.vector(data[which(data$choix==1),]$label[1]))
  dt_topics$label_df_modele_choix = subset(df_label_1, modele == input$Choix_modele & choix == 1)
  dt_topics$top_topic_terms_modele = subset(datas$top_topic_terms, modele == input$Choix_modele)
  dt_topics$df_document_vector_modele = subset(datas$df_document_vector, modele == input$Choix_modele)
  dt_topics$df_sim_doc_topic_modele = subset(datas$df_sim_doc_topic, modele == input$Choix_modele)
  dt_topics$label_df_choix = subset(df_label_1, choix == 1)
  
}, priority = 4,ignoreNULL = FALSE,ignoreInit = FALSE)

# retourner à la page 'thèmes'
observeEvent(input$bouton_retour, {
  updateTabsetPanel(session = session, inputId = "tabs_topics", selected = "Thèmes")
},ignoreNULL = FALSE,ignoreInit = TRUE)

# Si on sélectionne un topic, affiche la page 'Documents' pour le topic associé
observeEvent(input$select_button, {
  updateTabsetPanel(session = session, inputId = "tabs_topics", selected = "Documents")
  updatePickerInput(session = session, inputId = "Choix_themes", selected = as.vector(input$select_button))
  session$sendCustomMessage(type = 'resetInputValue', message =  "select_button")
},ignoreNULL = FALSE,ignoreInit = TRUE)

# Si on change theme dans 'Documents' alors changer theme dans 'NMF_topic'
observeEvent({input$Choix_themes}, {
  updatePickerInput(session = session, inputId = "Choix_themes_nmf",
                    choices = as.vector(dt_topics$label_df_modele_choix$label),
                    selected = as.vector(input$Choix_themes))
})

# vider tableau
observeEvent(input$Choix_themes_nmf, {
  output$table_topics_nmf <- DT::renderDataTable(
    {}, server = FALSE, escape = FALSE, selection = 'none'
  )
})

########################################################################################

plotly_bar_docs_nc = function(label_df, data_docs){
  "Nombre de documents dans chaque topic (représentation bar)"
  
  data_docs = merge(data_docs[,c('modele','cluster')],label_df[,c('label','cluster')])
  data_docs = sqldf("SELECT label, COUNT(modele) as count FROM data_docs GROUP BY label;")
  data_docs = data_docs[order(data_docs$count),]
  
  fig <- plot_ly(x = reorder(as.character(data_docs$label),rev(data_docs$count)),y = data_docs$count,
                 type = 'bar', color = I("rgba(153,51,204,1)") )%>%
    layout(title = 'Nombre de documents par thèmes')
  
  return(fig)
}

plot.bar = reactiveValues(out = NULL)

observe({
output$repartition_topics <- renderPlotly({
  plot.bar$out = plotly_bar_docs_nc(dt_topics$label_df_modele_choix, dt_topics$df_document_vector_modele)
  })
},priority = 1)

########################################################################################

shinyInput <- function(FUN, len, id, label, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(id[i], label[i],...))
  }
  inputs
}

dataframe_topics = function(nb_top_termes, label_df_choix,data_terms,data_docs,sim_topics){
  "Affiche les tops termes de chaque topic et classe les topics selon choix_classement"
  
   if (input$choix_classement == "Nombre de documents") {
     classement_cluster = as.data.frame(table(data_docs$cluster))
     classement_cluster = subset(classement_cluster, !(Var1 == -1)) #remove -1 cluster
     classement_cluster = classement_cluster[order(-classement_cluster$Freq),]
     names(classement_cluster) = c('cluster','score')
     name_score = "Nombre de documents "
   }
   else{
     classement_cluster = sim_topics[order(-sim_topics$sim),]
     names(classement_cluster) = c('cluster','score','modele')
     name_score = "Similarités "
   }
  
  idx_topics = classement_cluster$cluster
  nombre_labels = length(label_df_choix$label)
  
  numero = 1:nombre_labels
  label = character(nombre_labels)
  classement = character(nombre_labels)
  top_termes = character(nombre_labels)
  
  for (i in seq_len(nombre_labels)) {
    idx = idx_topics[i]
    
    label_i = label_df_choix[which(label_df_choix$cluster==idx),]$label
    label[i] = as.character(label_i)
      
    score_i = classement_cluster[which(classement_cluster$cluster==idx),]$score
    classement[i] = as.character(round(score_i,2))
      
      
    top_termes_i = data_terms[which(data_terms$cluster==idx),]
    top_termes_i = top_termes_i[order(-top_termes_i$freq),]$terms
      
    message = paste(top_termes_i[1])
    if (nb_top_termes > 1) {
      for (j in 2:nb_top_termes) {
        message = paste(message,' - ',top_termes_i[j])
      }
    }
    
    top_termes[i] = as.character(message)
  }
  
  return(list(numero,classement,label,top_termes))

}

df <- reactiveValues(data = NULL)

nombre_topics = function(label_df_choix){
  return(length(label_df_choix$label))
}

output$table_topics <- DT::renderDataTable(
  {
    label_df_choix = dt_topics$label_df_modele_choix
    data_terms = dt_topics$top_topic_terms_modele
    data_docs = dt_topics$df_document_vector_modele
    sim_topics = dt_topics$df_sim_doc_topic_modele
    
    list_df_top_termes = dataframe_topics(input$select_nb_topterms, label_df_choix,data_terms,data_docs,sim_topics)
    data.frame(
      
      Label = shinyInput(actionButton, length(list_df_top_termes[[3]]), 
                         list_df_top_termes[[3]], label = list_df_top_termes[[3]],
                         onclick = 'Shiny.onInputChange(\"select_button\",  this.id)',
                         style='text-align:center;width: 100%;background-color:rgba(153,51,204,0.5);color:black;border-radius: 12px;border: 2px solid #000000'),
      Classement = list_df_top_termes[[2]],
      Top_termes = list_df_top_termes[[4]],
      stringsAsFactors = FALSE,
      row.names = 1:length(list_df_top_termes[[3]])
    )
  }, server = FALSE, escape = FALSE, selection = 'none',
  options = list(lengthChange = FALSE, pageLength = nombre_topics(dt_topics$label_df_modele_choix), paging = FALSE, info = FALSE,
                 ordering = FALSE,columnDefs = list(list(className = 'dt-center', targets = 1:2)))
)

########################################################################################
output$nombre_documents <- renderText({
  "Nombre de documents dans tout le corpus"
  data_docs = dt_topics$df_document_vector_modele
  paste("Nombre de documents :", length(data_docs$terms))
})
########################################################################################
output$nombre_themes <- renderText({
  "Nombre de topics pour le modèle sélectionné"
  data_docs = dt_topics$df_document_vector_modele
  paste("Nombre de thèmes :", length(unique(data_docs$cluster)))
})
########################################################################################
########################################################################################
########################################################################################

top_termes_topic= function(nb_top_termes, top_termes_i){
  "Affiche les tops termes du topic numéro 'top_termes_i'"
  top_termes_i = top_termes_i$terms
  
  message = ''
  message = paste0(message,'<br/> <br/>','Top termes : <br/> <Font size=-1>',top_termes_i[1])
  if (nb_top_termes > 1) {
    for (j in 2:nb_top_termes) {
      message = paste(message,' - ',top_termes_i[j])
    }
  }
  message = paste(message,'</font> <br/> <br/> ')
  return(HTML(message))
}

output$top_termes <- renderText({
  top_termes_i = dt_topics$top_topic_terms_modele_cluster
  top_termes_topic(input$select_nb_topterms, top_termes_i)
})
########################################################################################

plotly_top_termes= function(nb_topterms, top_termes_i, df_word_cluster_tf_i){
  "Sous forme de bar, le nombre de fois où chaque top termes apparaît dans les documents du topics"
  #top_termes_i$terms = gsub("_"," ",top_termes_i$terms)
  
  df_word_cluster_tf_i = df_word_cluster_tf_i[,c('terms','freq')]
  names(df_word_cluster_tf_i) = c('terms','freq_tf')
  
  #top_termes_i = merge(top_termes_i,df_word_cluster_tf_i)
  
  #top_termes_i = top_termes_i[order(-top_termes_i$freq),][1:min(nb_topterms,10),]
  df_word_cluster_tf_i = df_word_cluster_tf_i[order(-df_word_cluster_tf_i$freq_tf),][1:10,]
  
  xform <- list(categoryorder = "array",
                categoryarray = c(as.character(df_word_cluster_tf_i$terms)))
  
  fig <- plot_ly(x = as.character(df_word_cluster_tf_i$terms),y = df_word_cluster_tf_i$freq_tf,
                 type = 'bar', color = I("rgba(153,51,204,1)"))%>% 
    layout(xaxis = xform)%>%
    layout(title = 'Fréquences des termes')
  
  return(fig)
}

output$freq_top_termes <- renderPlotly({
  top_termes_i = dt_topics$top_topic_terms_modele_cluster
  df_word_cluster_tf_i = dt_topics$df_word_cluster_tf_modele_cluster
  plotly_top_termes(input$select_nb_topterms, top_termes_i, df_word_cluster_tf_i)
})


########################################################################################
surligner_mots = function(nb_top_termes, lim_top_termes,label_i,top_termes_i,df_document_vector_reactive,df_word_cluster_i){
  "Affiche des documents du topic et Surligne les tops termes du topic"
  
  top_termes_i = top_termes_i$terms
  df_document_vector_i = subset(df_document_vector_reactive, cluster %in% label_i$cluster)
  df_document_vector_i = df_document_vector_i[1:5,]
  df_word_cluster_i = df_word_cluster_i[order(-df_word_cluster_i$freq),]
  label_i = label_i$label
  
  message = 'Exemples de documents appartenant au thème : <br/> <br/>'
  df_document_vector_i = strsplit(as.character(df_document_vector_i$terms), " ")
  
  for (text_split in df_document_vector_i) {
    html = "<span style='background-color:lavender'><Font size=-1>"
    for (j in 1:length(text_split)) {
      x = 0
      
      termes = c()
      if (length(text_split) > 2) {
        mots_3 = c()
        if (j < length(text_split) - 1) {
          mots_3 = c(mots_3,paste(text_split[j],text_split[j+1],text_split[j+2]))
        }
        if (j < length(text_split) & j != 0) {
          mots_3 = c(mots_3,paste(text_split[j-1],text_split[j],text_split[j+1]))
        }
        if (j > 1) {
          mots_3 = c(mots_3,paste(text_split[j-2],text_split[j-1],text_split[j]))
        }
        termes = c(termes,mots_3)
      }
      
      if (length(text_split) > 1) {
        mots_2 = c()
        if (j < length(text_split)) {
          mots_2 = c(mots_2,paste(text_split[j],text_split[j+1]))
        }
        if (j > 0) {
          mots_2 = c(mots_2,paste(text_split[j-1],text_split[j]))
        }
        termes = c(termes,mots_2)
      }  
      
      mots_1 = c(text_split[j])
      termes = c(termes,mots_1)
      
      for (terme in termes) {
        if (is_in(terme,df_word_cluster_i$terms[1:lim_top_termes])) {
          x = 0.5
        }
      }
      
      html = paste(html,' ')
      html = paste0(html,"<span style='background:rgba(153,51,204,",x,");font-family:monospace'>")
      html = paste0(html,text_split[j])
      html = paste0(html,"</span>")
    }
    html = paste0(html,"</font></span>")
    message = paste(message,html,'<br/> <br/>')
  }
  
  return(HTML(message))
}

output$exemple_docs <- renderText({
  label_i = dt_topics$df_label_modele_cluster
  top_termes_i = dt_topics$top_topic_terms_modele_cluster
  df_document_vector_reactive = dt_topics$df_document_vector_modele_cluster
  #if (input$Choix_modele == 'lda'){
  df_word_cluster_i = dt_topics$df_word_cluster_tf_modele_cluster
  #}
  #else{
  #df_word_cluster_i = dt_topics$df_word_cluster_tfidf_modele_cluster
  #}
  surligner_mots(input$select_nb_topterms,input$select_lim_top_termes,label_i,top_termes_i,df_document_vector_reactive,df_word_cluster_i)
})

########################################################################################
########################################################################################
########################################################################################

observeEvent(input$button_modele, {
  if (!is.null(input$button_modele)){
    updatePickerInput(session = session, inputId = "Choix_modele", selected = as.vector(input$button_modele))
    updateTabsetPanel(session = session, inputId = "tabs_topics", selected = "Thèmes")
    session$sendCustomMessage(type = 'resetInputValue', message =  "button_modele")
  }
},ignoreNULL = FALSE,ignoreInit = TRUE)

observeEvent({input$change_topic}, {
  info_df <- reactiveValues(data = data.frame(
    
    Modèle = shinyInput(actionButton, length(datas$df_informations[which(datas$df_informations$infos == "loss_fct"),]$modele), 
                        as.character(datas$df_informations[which(datas$df_informations$infos == "loss_fct"),]$modele),
                        label = as.character(datas$df_informations[which(datas$df_informations$infos == "loss_fct"),]$modele),
                       onclick = 'Shiny.onInputChange(\"button_modele\",  this.id)',
                       style='text-align:center;width: 100%;background-color:rgba(153,51,204,0.5);color:black;border-radius: 12px;border: 2px solid #000000'),
    Fonction_de_perte = as.character(round(datas$df_informations[which(datas$df_informations$infos == "loss_fct"),]$value,3))  #valeurs
  ))
  
  #Dataframe des modèles avec leur fonction de perte
  output$methodes <- DT::renderDataTable(
    info_df$data, server = FALSE, escape = FALSE, selection = 'none',
    options = list(lengthChange = FALSE, paging = FALSE, info = FALSE,searching = FALSE,
                   ordering = FALSE,columnDefs = list(list(className = 'dt-center', targets = 1:2)))
  )
},ignoreNULL = FALSE,ignoreInit = FALSE)
# output$methodes <- renderUI({
#   out=tagList()
#   df_score = df_informations[which(df_informations$infos == "fct_perte"),]
#   df_score = df_score[order(-df_score$valeurs),]
#   out=lapply(df_score$modele,  function(x){ 
#     tagList(fluidRow(
#       br(),
#       actionButton(paste0("bouton_",x), HTML(paste('<pre>',x,get_value(df_score, x),'</pre>')), 
#                   style='background-color:rgba(153,51,204,0.5)',
#                   onclick = paste0("Shiny.setInputValue('button_modele',this.innerText);")
#       )
#     ),
#     br())
#     })
#   
#   out
# })

dataframe_labels = function(subset_label){
  "Affiche un dataframe avec en colonne chaque modèles et leurs labels ligne par ligne"
  
  max_nb_label = 0
  for (modele in unique(df_informations$modele)) {
    len = length(subset_label[which(subset_label$modele == modele),]$label)
    if (len > max_nb_label) {
      max_nb_label = len
    }
  }
  dataframe = data.frame(numero = 1:max_nb_label)
  for (modele in unique(df_informations$modele)) {
    label_modele = as.vector(subset_label[which(subset_label$modele == modele),]$label)
    label_modele = as.data.frame(list(c(label_modele,rep('',max_nb_label - length(label_modele)))),col.names = modele)
    dataframe = cbind(dataframe,label_modele)
  }
  return(list(dataframe[-1],max_nb_label))
}

output$table_labels = renderDT(
  dataframe_labels(dt_topics$label_df_choix)[[1]],
  options = list(lengthChange = FALSE, pageLength = dataframe_labels(dt_topics$label_df_choix)[[2]], paging = FALSE, info = FALSE,
                 ordering = FALSE)
)