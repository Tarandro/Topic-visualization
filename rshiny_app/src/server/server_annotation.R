dt_ann = reactiveValues(label_df_modele_choix = NULL, df_word_cluster_tf_modele = NULL, df_document_vector_modele = NULL, df_document_vector_modele_sample = NULL,
                        select_button_ann = FALSE, list_highlights = list(), df_annotated_modele_sample = NULL, no_highlights = NULL)

# udpate si on change de label pour un topic
observeEvent({input$Choix_modele_ann
  input$boutons_label
  input$change_topic}, {
    dt_ann$label_df_modele_choix = subset(dt_topics$label_df_choix, modele == input$Choix_modele_ann)
    dt_ann$df_word_cluster_tf_modele = subset(datas$df_word_cluster_tf, modele == input$Choix_modele_ann)
    dt_ann$df_document_vector_modele = subset(datas$df_document_vector, modele == input$Choix_modele_ann)

    output$ui_max_topic <- renderUI({
      sliderInput("select_max_topic_ann", label = "Limite du nombre de top topics", min = 1, 
                  max = length(dt_ann$label_df_modele_choix$label), value = length(dt_ann$label_df_modele_choix$label), round = T, step = 1)
    })
    
    
  },priority = 2,ignoreNULL = FALSE,ignoreInit = FALSE)

# sélectionne le bouton rerun
observeEvent({input$bouton_rerun_ann
  input$Choix_modele_ann
  input$bouton_save_ann},{
    # take only text with n_char > mean(nchar)
    dt_ann$df_document_vector_modele_sample = dt_ann$df_document_vector_modele[nchar(dt_ann$df_document_vector_modele$terms_non_pre) > mean(nchar(dt_ann$df_document_vector_modele$terms_non_pre)),]
    dt_ann$df_document_vector_modele_sample = dt_ann$df_document_vector_modele_sample[which(dt_ann$df_document_vector_modele_sample$annotated == ""),]
    if (nrow(dt_ann$df_document_vector_modele_sample) > 0) {
      dt_ann$df_document_vector_modele_sample = dt_ann$df_document_vector_modele_sample[sample(1:length(dt_ann$df_document_vector_modele_sample$terms)),]
    }
    },priority = 1,ignoreNULL = FALSE,ignoreInit = FALSE)

# observeEvent(input$select_button_doc, {
#   dt_ann$select_button_doc = TRUE
#   updateTabItems(session, "tabs", "topics")
#   updateTabsetPanel(session = session, inputId = "tabs_topics", selected = "Documents")
#   updatePickerInput(session = session, inputId = "Choix_modele", selected = as.vector(input$Choix_modele_ann))
#   updatePickerInput(session = session, inputId = "Choix_themes", choices = as.vector(dt_ann$label_df_modele_choix$label), selected = as.vector(input$select_button_doc))
#   session$sendCustomMessage(type = 'resetInputValue', message =  "select_button_doc")
# },priority = 5,ignoreNULL = TRUE,ignoreInit = TRUE)

#######################################################################################

button_topic = function(lim_top_termes, subset_df_label, subset_df_document_vector, df_word_cluster, highlights, no_highlights, save = FALSE){
  
  if (nrow(subset_df_document_vector) == 0) {
    return(NULL)
  }
  
  columns_names = paste0('X',subset_df_label$cluster)
  subset_df_document_vector_col = subset_df_document_vector[,columns_names]
  subset_df_text =  subset_df_document_vector[,1]
  number_line = rownames(subset_df_document_vector)[1]
  
  message_button = ""
  message_text = ""
  nb_example = 0
  idx_text = 1
  while(nb_example < 1 & idx_text <= length(subset_df_text)) {
    text_split = str_split(subset_df_text[idx_text], ' ')[[1]]
    
    text_lemmatize = copy(text_split)
    for (word in 1:length(text_split)) {
      text_lemmatize[word] = subset(df_lemma, token == text_split[word])$lem[1]
    }
    
    color = rep('',length(text_split))
    color_tf_coef = rep(0,length(text_split))
    sum_proba = 0
    number_of_topic = 0
    html = "Topics : <br/> <br/> "
    
    if (is.null(input$select_max_topic_ann)) {
      n_max_topics <- length(subset_df_label$label)
    }else{
      n_max_topics <- input$select_max_topic_ann
    }
    
    for (nb_topic in 1:n_max_topics) {
      
      proba = max(subset_df_document_vector_col[idx_text,])
      sum_proba = sum_proba + proba
      
      number_of_topic = number_of_topic + 1

      col = argmax(subset_df_document_vector_col[idx_text,])[[1]]
      name_col = columns_names[col]
      # label :
      #html = paste0(html,"<span style='text-align:center;width: 100%;background-color:",all_color[col],";color:black;border-radius: 5px;border: 2px solid #000000'>")
      #html = paste0(html,' &nbsp; ',subset_df_label$label[col], ' (',round(proba,3),') &nbsp;')
      #html = paste0(html,"</span>", ' ')
      subset_df_document_vector_col[idx_text,col] = -1
        
      html = paste(html, as.character(actionButton(subset_df_label$label[col], paste0(' ',subset_df_label$label[col],' (',round(proba,3),') '),
                                                     onclick = paste0("Shiny.onInputChange(\'select_button_ann\','",subset_df_label$label[col],"')"),
                                                     style=paste0("text-align:center;width: 45%;background-color:",all_color[col],";color:black;border-radius: 5px;border: 2px solid #000000"))))
        
      if (nb_topic%%2==0) {
        html = paste(html, "<br/> <br/>")
      }
      
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
    
    # add annotation :
    for (col in names(highlights)) {
      for (highlight_text in highlights[[col]]) {
        
        n=length(str_split(highlight_text, ' ')[[1]])
        nb_max_na = n+1
        for (i in 1:length(text_split)) {
          if (!is.na(text_split[i])) {
            j = i
            condition = F
            if (n==1) {
              condition = T
            }else{
              while (condition == F & j < i + nb_max_na) {
                j = j+1
                if (sum(!is.na(text_split[i:j]))==n) {
                  condition = T
                }
              }
            }
            
            if (condition) {
              ngrams = text_split[i]
              if (n > 1) {
                for (word in text_split[(i+1):j]) {
                  if (!is.na(word)) {
                    ngrams = paste(ngrams, word)
                  }
                }
              }
              
              if (ngrams == highlight_text) {
                for (idx in i:j) {
                  color[idx] = all_color[as.integer(col)]
                }
              }
            }
          }
        }
        
      }
    }
    
    # no highlights :
    for (no_highlight_text in no_highlights) {
        
        n=length(str_split(no_highlight_text, ' ')[[1]])
        nb_max_na = n+1
        for (i in 1:length(text_split)) {
          if (!is.na(text_split[i])) {
            j = i
            condition = F
            if (n==1) {
              condition = T
            }else{
              while (condition == F & j < i + nb_max_na) {
                j = j+1
                if (sum(!is.na(text_split[i:j]))==n) {
                  condition = T
                }
              }
            }
            
            if (condition) {
              ngrams = text_split[i]
              if (n > 1) {
                for (word in text_split[(i+1):j]) {
                  if (!is.na(word)) {
                    ngrams = paste(ngrams, word)
                  }
                }
              }
              
              if (ngrams == no_highlight_text) {
                for (idx in i:j) {
                  color[idx] = ''
                }
              }
            }
          }
        }
        
      }
    
    
    
    message_button = html
    
    # sum_proba > 0.8 et au moins un mot surligner pour chaque topic :
    if (sum_proba > 0.0) {
      message_text = paste0(message_text," <br/> <br/> ","<span style='background-color:lavender'><Font size=+2>")
      for (j in 1:length(text_split)) {
        message_text = paste(message_text,' ')
        message_text = paste0(message_text,"<span style='background:",color[j],";font-family:monospace'>")
        message_text = paste0(message_text,text_split[j])
        message_text = paste0(message_text,"</span>")
      }
      message_text = paste0(message_text,"</font></span>")
      
      if (save) {
        datas$df_document_vector[which(rownames(datas$df_document_vector) == number_line), which(colnames(datas$df_document_vector) == "annotated")] = message_text
        dt_ann$df_document_vector_modele[which(rownames(dt_ann$df_document_vector_modele) == number_line), which(colnames(dt_ann$df_document_vector_modele) == "annotated")] = message_text
      }
      
      nb_example = nb_example + 1
    }
    idx_text = idx_text + 1
  }
  return(list(message_button = HTML(message_button), message_text = HTML(message_text)))
}

observeEvent(c(input$bouton_rerun_ann,input$Choix_modele_ann, input$bouton_save_ann),{
  dt_ann$list_highlights <- list()
  dt_ann$no_highlights <- NULL
    
  # input$select_lim_top_termes_ann
  l <- button_topic(100, dt_ann$label_df_modele_choix, dt_ann$df_document_vector_modele_sample,
                    dt_ann$df_word_cluster_tf_modele, dt_ann$list_highlights, dt_ann$no_highlights)
  
  output$html_button_topic <- renderText({
    l$message_button
  })
  
  output$html_show_text <- renderText({
    l$message_text
  })
  
},priority = 0,ignoreNULL = FALSE,ignoreInit = FALSE)




observeEvent(input$select_button_ann, {
  dt_ann$select_button_ann = TRUE
  
  highlight_text <- input$mydata
  
  # remove space/punctuation at the start and end
  while (length(highlight_text) > 0 & substr(highlight_text,1,1) %in% c(" ", ".", ",", "?", ";", ":", "/", "!")) {
    highlight_text <- substr(highlight_text,2,nchar(highlight_text))
  }
  while (length(highlight_text) > 0 & str_sub(highlight_text,-1,-1) %in% c(" ", ".", ",", "?", ";", ":", "/", "!")) {
    highlight_text <- str_sub(highlight_text,0, -2)
  }
  
  if (nchar(highlight_text) > 0) {
    
    # Remove highlight_text for each topic:
    for (idx in names(dt_ann$list_highlights)) {
      dt_ann$list_highlights[[idx]] <- dt_ann$list_highlights[[idx]][dt_ann$list_highlights[[idx]] != highlight_text]
    }
    dt_ann$no_highlights <- dt_ann$no_highlights[dt_ann$no_highlights != highlight_text]
    
    # Add highlight_text to the selected topic
    idx_label <- which(dt_ann$label_df_modele_choix == input$select_button_ann)
    if (as.character(idx_label) %in% names(dt_ann$list_highlights)) {
      dt_ann$list_highlights[[as.character(idx_label)]] <- c(dt_ann$list_highlights[[as.character(idx_label)]], highlight_text)
    }else{
      dt_ann$list_highlights[[as.character(idx_label)]] <- highlight_text
    }
  }
  
  l <- button_topic(100, dt_ann$label_df_modele_choix, dt_ann$df_document_vector_modele_sample,
                    dt_ann$df_word_cluster_tf_modele, dt_ann$list_highlights, dt_ann$no_highlights)

  output$html_show_text <- renderText({
    l$message_text
  })
  
  session$sendCustomMessage(type = 'resetInputValue', message =  "select_button_ann")
  
},priority = 5,ignoreNULL = TRUE,ignoreInit = TRUE)


observeEvent(input$bouton_white_ann, {
  
  highlight_text <- input$mydata
  
  # remove space/punctuation at the start and end
  while (length(highlight_text) > 0 & substr(highlight_text,1,1) %in% c(" ", ".", ",", "?", ";", ":", "/", "!")) {
    highlight_text <- substr(highlight_text,2,nchar(highlight_text))
  }
  while (length(highlight_text) > 0 & str_sub(highlight_text,-1,-1) %in% c(" ", ".", ",", "?", ";", ":", "/", "!")) {
    highlight_text <- str_sub(highlight_text,0, -2)
  }
  
  if (nchar(highlight_text) > 0) {
    
    # Remove highlight_text for each topic:
    for (idx in names(dt_ann$list_highlights)) {
      dt_ann$list_highlights[[idx]] <- dt_ann$list_highlights[[idx]][dt_ann$list_highlights[[idx]] != highlight_text]
    }
    
    # Add highlight_text to the null topic
    dt_ann$no_highlights <- unique(c(dt_ann$no_highlights, highlight_text))
  }
  
  l <- button_topic(100, dt_ann$label_df_modele_choix, dt_ann$df_document_vector_modele_sample,
                    dt_ann$df_word_cluster_tf_modele, dt_ann$list_highlights, dt_ann$no_highlights)
  
  output$html_show_text <- renderText({
    l$message_text
  })
  
},priority = 5,ignoreNULL = TRUE,ignoreInit = TRUE)


observeEvent(c(input$select_max_topic_ann, input$boutons_label),{
  req(dt_ann$label_df_modele_choix)
  req(dt_ann$df_document_vector_modele_sample)
  l <- button_topic(100, dt_ann$label_df_modele_choix, dt_ann$df_document_vector_modele_sample, 
                    dt_ann$df_word_cluster_tf_modele, dt_ann$list_highlights, dt_ann$no_highlights)
  output$html_button_topic <- renderText({
    l$message_button
  })
  
  output$html_show_text <- renderText({
    l$message_text
  })
},priority = 10)




observeEvent(input$bouton_save_ann,{
  
  l <- button_topic(100, dt_ann$label_df_modele_choix, dt_ann$df_document_vector_modele_sample, 
                    dt_ann$df_word_cluster_tf_modele, dt_ann$list_highlights, dt_ann$no_highlights, TRUE)
  
},priority = 10)

#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################

# sélectionne le bouton rerun
observeEvent({input$bouton_rerun_ann_ex
  input$Choix_modele_ann
  input$bouton_save_ann},{
    dt_ann$df_annotated_modele_sample = dt_ann$df_document_vector_modele[which(dt_ann$df_document_vector_modele$annotated != ""),]
    if (nrow(dt_ann$df_annotated_modele_sample) > 0) {
      dt_ann$df_annotated_modele_sample = dt_ann$df_annotated_modele_sample[sample(1:length(dt_ann$df_annotated_modele_sample$terms)),]
    }
  },priority = 1,ignoreNULL = FALSE,ignoreInit = FALSE)


annotated_examples = function(lim_top_termes, subset_df_label, subset_df_document_vector){
  
  if (nrow(subset_df_document_vector) == 0) {
    return(NULL)
  }

  subset_df_text =  subset_df_document_vector$annotated
  
  message = ""
  nb_example = 0
  idx_text = 1
  while(nb_example < 10 & idx_text <= length(subset_df_text)) {
    text = subset_df_text[idx_text]
    
    html = "Topics : <br/> <br/> "
    
    for (nb_topic in 1:length(subset_df_label$label)) {
      col = nb_topic
      
      if (grepl(all_color[col], text, fixed = TRUE)) {
        html = paste(html, as.character(actionButton(subset_df_label$label[col], paste0(' ',subset_df_label$label[col]),
                                                     onclick = paste0("Shiny.onInputChange(\'select_button_ann_ex\','",subset_df_label$label[col],"')"),
                                                     style=paste0("text-align:center;width: 20%;background-color:",all_color[col],
                                                                  ";color:black;border-radius: 5px;border: 2px solid #000000"))))
      }
    }
    
    html = paste0(html," <br/> ")
    
    html = paste(html,' ')
    html = paste0(html,text)
      
    message = paste(message,html,'<br/> <br/> <br/>')
    nb_example = nb_example + 1
    idx_text = idx_text + 1
  }
  return(message)
}

observeEvent(c(input$bouton_rerun_ann_ex,input$Choix_modele_ann, input$bouton_save_ann),{
  dt_ann$list_highlights <- list()
  
  message <- annotated_examples(input$select_lim_top_termes_ann, dt_ann$label_df_modele_choix, dt_ann$df_annotated_modele_sample)
  
  output$example_documents_ann <- renderText({message})
  
},priority = 0,ignoreNULL = FALSE,ignoreInit = FALSE)