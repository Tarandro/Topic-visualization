dt_docs = reactiveValues(label_df_modele_choix = NULL, df_word_cluster_tf_modele = NULL, df_document_vector_modele = NULL, df_document_vector_modele_sample = NULL,
                         select_button_doc = FALSE)

# udpate si on change de label pour un topic
observeEvent({input$Choix_modele_doc
  input$boutons_label
  input$change_topic}, {
    dt_docs$label_df_modele_choix = subset(dt_topics$label_df_choix, modele == input$Choix_modele_doc)
    dt_docs$df_word_cluster_tf_modele = subset(datas$df_word_cluster_tf, modele == input$Choix_modele_doc)
    dt_docs$df_document_vector_modele = subset(datas$df_document_vector, modele == input$Choix_modele_doc)
  },priority = 2,ignoreNULL = FALSE,ignoreInit = FALSE)

# sélectionne le bouton rerun
observeEvent({input$bouton_rerun_doc
  input$Choix_modele_doc},{
    # take only text with n_char > mean(nchar)
    dt_docs$df_document_vector_modele_sample = dt_docs$df_document_vector_modele[nchar(dt_docs$df_document_vector_modele$terms_non_pre) > mean(nchar(dt_docs$df_document_vector_modele$terms_non_pre)),]
    dt_docs$df_document_vector_modele_sample = dt_docs$df_document_vector_modele_sample[sample(1:length(dt_docs$df_document_vector_modele_sample$terms)),]
  },priority = 0,ignoreNULL = FALSE,ignoreInit = FALSE)

observeEvent(input$select_button_doc, {
  dt_docs$select_button_doc = TRUE
  updateTabItems(session, "tabs", "topics")
  updateTabsetPanel(session = session, inputId = "tabs_topics", selected = "Documents")
  updatePickerInput(session = session, inputId = "Choix_modele", selected = as.vector(input$Choix_modele_doc))
  updatePickerInput(session = session, inputId = "Choix_themes", choices = as.vector(dt_docs$label_df_modele_choix$label), selected = as.vector(input$select_button_doc))
  session$sendCustomMessage(type = 'resetInputValue', message =  "select_button_doc")
},priority = 5,ignoreNULL = TRUE,ignoreInit = TRUE)

all_color = c("#FFFF00", "#1CE6FF", "#FF34FF", "#FF4A46", "#008941", "#006FA6", "#A30059",
              "#FFDBE5", "#63FFAC", "#B79762", "#8FB0FF",
              "#FEFFE6", "#4FC601", "#3B5DFF", "#FF2F80",
              "#BA0900", "#6B7900", "#00C2A0", "#FFAA92", "#FF90C9", "#B903AA", "#D16100",
              "#DDEFFF", "#7B4F4B", "#A1C299", "#0AA6D8", "#00846F",
              "#FFB500", "#C2FFED", "#A079BF", "#CC0744", "#C0B9B2", "#C2FF99",
              "#00489C", "#0CBD66", "#EEC3FF", "#456D75", "#B77B68", "#7A87A1", "#788D66",
              "#885578", "#FAD09F", "#FF8A9A", "#D157A0", "#BEC459", "#456648", "#0086ED", "#886F4C",
              
              "#B4A8BD", "#00A6AA", "#636375", "#A3C8C9", "#FF913F", "#938A81", "#809693",
              "#00FECF", "#B05B6F", "#8CD0FF", "#3B9700", "#04F757", "#C8A1A1", "#1E6E00",
              "#7900D7", "#A77500", "#6367A9", "#A05837", "#D790FF", "#9B9700", "#61615A", 
              "#549E79", "#FFF69F", "#72418F", "#BC23FF", "#99ADC0", "#922329",
              "#FDE8DC", "#404E55", "#0089A3", "#CB7E98", "#A4E804", "#324E72",
              "#83AB58", "#D1F7CE", "#C8D0F6", "#A3A489", "#806C66", "#997D87",
              "#BF5650", "#E83000", "#66796D", "#DA007C", "#FF1A59", "#8ADBB4",
              "#C895C5", "#FF6832", "#66E1D3", "#CFCDAC", "#D0AC94", "#7ED379")
#######################################################################################

shinyInput <- function(FUN, len, id, label, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(id[i], label[i],...))
  }
  inputs
}

#shinyInput(actionButton, length(list_df_top_termes[[3]]), 
#           list_df_top_termes[[3]], label = list_df_top_termes[[3]],
#           onclick = 'Shiny.onInputChange(\"select_button\",  this.id)',
#           style='text-align:center;width: 100%;background-color:rgba(153,51,204,0.5);color:black;border-radius: 12px;border: 2px solid #000000')

example_documents = function(lim_top_termes, subset_df_label, subset_df_document_vector, df_word_cluster){
  
  columns_names = paste0('X',subset_df_label$cluster)
  subset_df_document_vector_col = subset_df_document_vector[,columns_names]
  subset_df_text =  subset_df_document_vector[,1]
  
  message = ""
  nb_example = 0
  idx_text = 1
  while(nb_example < 5 & idx_text < length(subset_df_text)) {
    text_split = str_split(subset_df_text[idx_text], ' ')[[1]]
    
    text_lemmatize = copy(text_split)
    for (word in 1:length(text_split)) {
      text_lemmatize[word] = subset(df_lemma, token == text_split[word])$lem[1]
    }
    
    color = rep('',length(text_split))
    color_tf_coef = rep(0,length(text_split))
    sum_proba = 0
    number_of_topic = 0
    html = "Topics : "
    for (nb_topic in 1:3) {
      proba = max(subset_df_document_vector_col[idx_text,])
      sum_proba = sum_proba + proba
      if (proba > 0.1 ) {
        number_of_topic = number_of_topic + 1
        
        col = argmax(subset_df_document_vector_col[idx_text,])[[1]]
        name_col = columns_names[col]
        # label :
        #html = paste0(html,"<span style='text-align:center;width: 100%;background-color:",all_color[col],";color:black;border-radius: 5px;border: 2px solid #000000'>")
        #html = paste0(html,' &nbsp; ',subset_df_label$label[col], ' (',round(proba,3),') &nbsp;')
        #html = paste0(html,"</span>", ' ')
        subset_df_document_vector_col[idx_text,col] = -1
        
        html = paste(html, as.character(actionButton(subset_df_label$label[col], paste0(' ',subset_df_label$label[col],' (',round(proba,3),') '),
                                  onclick = paste0("Shiny.onInputChange(\'select_button_doc\','",subset_df_label$label[col],"')"),
                                  style=paste0("text-align:center;width: 20%;background-color:",all_color[col],";color:black;border-radius: 5px;border: 2px solid #000000"))))
        
        # top terms à surligner
        df_word_cluster_i = df_word_cluster[,c('terms',columns_names[col])]
        df_word_cluster_i = df_word_cluster_i[order(-df_word_cluster_i[,columns_names[col]]),]
        top_terms = df_word_cluster_i$terms[1:lim_top_termes]
        
        ## tri-grams :
        trigrams=3
        nb_max_na = 4
        for (i in 1:length(text_lemmatize)) {
          if (!is.na(text_lemmatize[i])) {
            j = i
            condition = F
            while (condition == F & j < i + nb_max_na) {
              j = j+1
              
              if (sum(!is.na(text_lemmatize[i:j]))==trigrams) {
                condition = T
              }
            }
            
            if (condition) {
              ngrams = text_lemmatize[i]
              for (word in text_lemmatize[(i+1):j]) {
                if (!is.na(word)) {
                  ngrams = paste(ngrams, word)
                }
              }
              if (is_in(ngrams,top_terms)) {
                tf_coef_ngrams = df_word_cluster_i[df_word_cluster_i$terms == ngrams, columns_names[col]][1]
                for (idx in i:j) {
                  if (tf_coef_ngrams > color_tf_coef[idx]) {
                    color[idx] = all_color[col]
                    color_tf_coef[idx] = tf_coef_ngrams
                  }
                }
              }
            }
          }
        }
        
        # bi-grams:
        bigrams=2
        nb_max_na = 3
        for (i in 1:length(text_lemmatize)) {
          if (!is.na(text_lemmatize[i])) {
            j = i
            condition = F
            while (condition == F & j < i + nb_max_na) {
              j = j+1
              if (sum(!is.na(text_lemmatize[i:j]))==bigrams) {
                condition = T
              }
            }
            
            if (condition) {
              ngrams = text_lemmatize[i]
              for (word in text_lemmatize[(i+1):j]) {
                if (!is.na(word)) {
                  ngrams = paste(ngrams, word)
                }
              }
              if (is_in(ngrams,top_terms)) {
                tf_coef_ngrams = df_word_cluster_i[df_word_cluster_i$terms == ngrams, columns_names[col]][1]
                for (idx in i:j) {
                  if (tf_coef_ngrams > color_tf_coef[idx]) {
                    color[idx] = all_color[col]
                    color_tf_coef[idx] = tf_coef_ngrams
                  }
                }
              }
            }
          }
        }
        
        #unigrams :
        for (i in 1:length(text_lemmatize)) {
          if (!is.na(text_lemmatize[i])) {
            if (is_in(text_lemmatize[i],top_terms)) {
              tf_coef_ngrams = df_word_cluster_i[df_word_cluster_i$terms == text_lemmatize[i], columns_names[col]][1]
              if (tf_coef_ngrams > color_tf_coef[i]) {
                color[i] = all_color[col]
                color_tf_coef[idx] = tf_coef_ngrams
              }
            }
          }
        }
        
      }
    }
    
    # sum_proba > 0.8 et au moins un mot surligner pour chaque topic :
    if (sum_proba > 0.8 & length(unique(color[!color==''])) == number_of_topic) {
      html = paste0(html," <br/> <br/> ","<span style='background-color:lavender'><Font size=-1>")
      for (j in 1:length(text_split)) {
        html = paste(html,' ')
        html = paste0(html,"<span style='background:",color[j],";font-family:monospace'>")
        html = paste0(html,text_split[j])
        html = paste0(html,"</span>")
      }
      html = paste0(html,"</font></span>")

      message = paste(message,html,'<br/> <br/> <br/>')
      nb_example = nb_example + 1
    }
    idx_text = idx_text + 1
  }
  return(HTML(message))
}


output$example_documents <- renderText({
  example_documents(input$select_lim_top_termes_doc, dt_docs$label_df_modele_choix, dt_docs$df_document_vector_modele_sample, dt_docs$df_word_cluster_tf_modele)
})