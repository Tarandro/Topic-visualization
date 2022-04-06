dt_ann = reactiveValues(label_df_modele_choix = NULL, df_word_cluster_tf_modele = NULL, df_document_vector_modele = NULL, df_document_vector_modele_sample = NULL,
                        select_button_ann = NULL, list_highlights = list(), df_annotated_modele_sample = NULL, no_highlights = NULL,
                        highlight_top_terms = list(), start_popup1 = TRUE, start_popup2 = 100)

# udpate si on change de label pour un topic
observeEvent({input$Choix_modele_ann
  input$boutons_label
  input$change_topic}, {
    
    dt_ann$label_df_modele_choix = subset(dt_topics$label_df_choix, modele == input$Choix_modele_ann)
    dt_ann$df_word_cluster_tf_modele = subset(datas$df_word_cluster_tf, modele == input$Choix_modele_ann)
    dt_ann$df_document_vector_modele = subset(datas$df_document_vector, modele == input$Choix_modele_ann)
    
    dt <- datas$dataset_preprocessed[which(datas$dataset_preprocessed$body_as_anonymised != ""),][, c("body_as_text", "body_as_anonymised")]
    colnames(dt) <- c("body_as_text", "terms_non_pre")
    dt_ann$df_document_vector_modele<-merge(x=dt_ann$df_document_vector_modele,y=dt,by="terms_non_pre",all.x=TRUE)

    output$ui_max_topic <- renderUI({
      sliderInput("select_max_topic_ann", label = "Limite du nombre de top topics", min = 1, 
                  max = length(dt_ann$label_df_modele_choix$label), value = length(dt_ann$label_df_modele_choix$label), round = T, step = 1)
    })
    
  },priority = 2,ignoreNULL = FALSE,ignoreInit = FALSE)

observeEvent(input$Choix_modele_ann, {
  dt_ann$highlight_top_terms <- list()
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
    }else{
      dt_ann$df_document_vector_modele_sample = dt_ann$df_document_vector_modele[nchar(dt_ann$df_document_vector_modele$terms_non_pre) > mean(nchar(dt_ann$df_document_vector_modele$terms_non_pre)),]
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

button_topic = function(lim_top_termes, subset_df_label, subset_df_document_vector, df_word_cluster, highlights, no_highlights, save = FALSE,
                        max_example = 1){
  
  if (nrow(subset_df_document_vector) == 0) {
    return(NULL)
  }
  
  columns_names = paste0('X',subset_df_label$cluster)
  subset_df_document_vector_col = subset_df_document_vector[,columns_names]
  subset_df_text =  subset_df_document_vector$body_as_text

  nb_example = 0
  idx_text = 1
  while(nb_example < max_example & idx_text <= length(subset_df_text)) {
    message_button = ""
    message_text = ""
    text_split = str_split(subset_df_text[idx_text], ' ')[[1]]
    text_split[text_split != ""]
    
    number_line = rownames(subset_df_document_vector)[idx_text]
    
    if (nb_example%%100 == 1) {
      print(paste(nb_example, "/", length(subset_df_text)))
    }
    
    text_lemmatize = copy(text_split)
    for (i in 1:length(text_split)) {
      word <- tolower(text_split[i])
      word <- gsub('[[:punct:] ]+',' ',word)
      word <- stringr::str_trim(word, "both")
      text_lemmatize[i] = subset(df_lemma, token == word)$lem[1]
    }
    
    color = rep('',length(text_split))
    color_tf_coef = rep(0,length(text_split))
    sum_proba = 0
    number_of_topic = 0
    html = "Topics : <br/> <br/> "
    
    if (is.null(input$select_max_topic_ann)) {
      n_max_topics <- length(subset_df_label$label)
    }else{
      n_max_topics <- min(length(subset_df_label$label), input$select_max_topic_ann)
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
      
      if (as.character(col) %in% names(dt_ann$highlight_top_terms)) {
        top_terms = dt_ann$highlight_top_terms[[as.character(col)]]
      }else{
        top_terms = df_word_cluster_i$terms[1:lim_top_termes] 
      }
        
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
              
              ngrams_without_punctuation <- gsub('[[:punct:] ]+',' ',ngrams)
              ngrams_without_punctuation <- stringr::str_trim(ngrams_without_punctuation, "both")
              
              highlight_text_without_punctuation <- gsub('[[:punct:] ]+',' ',highlight_text)
              highlight_text_without_punctuation <- stringr::str_trim(highlight_text_without_punctuation, "both")
              
              if (tolower(ngrams) == tolower(highlight_text) || tolower(ngrams_without_punctuation) == tolower(highlight_text) ||
                  tolower(ngrams_without_punctuation) == tolower(highlight_text_without_punctuation)) {
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
              ngrams_without_punctuation <- gsub('[[:punct:] ]+',' ',ngrams)
              ngrams_without_punctuation <- stringr::str_trim(ngrams_without_punctuation, "both")
              
              no_highlight_text_without_punctuation <- gsub('[[:punct:] ]+',' ',no_highlight_text)
              no_highlight_text_without_punctuation <- stringr::str_trim(no_highlight_text_without_punctuation, "both")
              
              if (tolower(ngrams) == tolower(no_highlight_text) || tolower(ngrams_without_punctuation) == tolower(no_highlight_text)||
                  tolower(ngrams_without_punctuation) == tolower(no_highlight_text_without_punctuation)) {
                for (idx in i:j) {
                  color[idx] = ''
                }
              }
            }
          }
        }
        
      }
    
    
    
    message_button = html
    
    # surligner pour chaque topic :
    message_text = paste0(message_text," <br/> <br/> ","<span style='background-color:lavender;font-family:monospace'><Font size=+2>")
    for (j in 1:length(text_split)) {
      message_text = paste(message_text,' ')
      text_to_highlight <- text_split[j]

      if (color[j] != "") {
        message_text = paste0(message_text,"<span style='background:",color[j],"'>")
        text_not_to_highlight <- ""
        while (length(text_to_highlight) > 0 & str_sub(text_to_highlight,-1,-1) %in% c(" ", ".", ",", "?", ";", ":", "/", "!")) {
          text_not_to_highlight <- paste0(str_sub(text_to_highlight,-1, -1), text_not_to_highlight)
          text_to_highlight <- str_sub(text_to_highlight,0, -2)
        }
        message_text = paste0(message_text, text_to_highlight)
        message_text = paste0(message_text,"</span>")
        if (text_not_to_highlight != "") {
          message_text = paste0(message_text,"<span style='background:","","'>",text_not_to_highlight,"</span>")
        }
      }else{
        message_text = paste0(message_text, text_to_highlight)
      }
      
    }
    message_text = paste0(message_text,"</font></span>")
      
    if (save) {
      datas$df_document_vector[which(rownames(datas$df_document_vector) == number_line), which(colnames(datas$df_document_vector) == "annotated")] = message_text
      dt_ann$df_document_vector_modele[which(rownames(dt_ann$df_document_vector_modele) == number_line), which(colnames(dt_ann$df_document_vector_modele) == "annotated")] = message_text
      if (nb_example > 0 & nb_example%%1000 == 0) {
        write.csv(dt_ann$df_document_vector_modele[,c("body_as_text", "annotated", columns_names)], file = paste0(path, 'annotated_dataset.csv'))
      }
    }
      
    nb_example = nb_example + 1
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
  
  highlight_text <- input$mydata
  
  # remove space at the start and end
  highlight_text <- stringr::str_trim(highlight_text, "both")
  
  # remove space/punctuation at the start and end
  highlight_text_without_punctuation <- gsub('[[:punct:] ]+',' ',highlight_text)
  highlight_text_without_punctuation <- stringr::str_trim(highlight_text_without_punctuation, "both")
  
  if (nchar(highlight_text) > 0) {
    
    # Remove highlight_text for each topic:
    for (idx in names(dt_ann$list_highlights)) {
      dt_ann$list_highlights[[idx]] <- dt_ann$list_highlights[[idx]][!(dt_ann$list_highlights[[idx]] %in% c(highlight_text,highlight_text_without_punctuation))]
    }
    dt_ann$no_highlights <- dt_ann$no_highlights[!(dt_ann$no_highlights %in% c(highlight_text,highlight_text_without_punctuation))]
    # Any no_highlight has to contain the word :
    dt_ann$no_highlights <- stringr::str_trim(unlist(str_split(dt_ann$no_highlights, highlight_text)), "both")
    dt_ann$no_highlights <- stringr::str_trim(unlist(str_split(dt_ann$no_highlights, highlight_text_without_punctuation)), "both")
    
    
    # Add highlight_text to the selected topic
    idx_label <- which(dt_ann$label_df_modele_choix == input$select_button_ann)
    if (as.character(idx_label) %in% names(dt_ann$list_highlights)) {
      dt_ann$list_highlights[[as.character(idx_label)]] <- c(dt_ann$list_highlights[[as.character(idx_label)]], highlight_text)
    }else{
      dt_ann$list_highlights[[as.character(idx_label)]] <- highlight_text
    }
    
    l <- button_topic(100, dt_ann$label_df_modele_choix, dt_ann$df_document_vector_modele_sample,
                      dt_ann$df_word_cluster_tf_modele, dt_ann$list_highlights, dt_ann$no_highlights)
    
    output$html_show_text <- renderText({
      l$message_text
    })
  }
  else{
    idx_label <- which(dt_ann$label_df_modele_choix == input$select_button_ann)
    columns_names = paste0('X',dt_ann$label_df_modele_choix$cluster)
    df_word_cluster_i <- dt_ann$df_word_cluster_tf_modele[,c('terms',columns_names[idx_label])]
    df_word_cluster_i <- df_word_cluster_i[order(-df_word_cluster_i[,columns_names[idx_label]]),]
    top_terms <- df_word_cluster_i$terms[1:100]
    if (idx_label %in% names(dt_ann$highlight_top_terms)) {
      selected_top_terms <- dt_ann$highlight_top_terms[[as.character(idx_label)]]
    }else{
      selected_top_terms <- top_terms
    }

    # Pop-up to select top terms to highlight
    showModal(modalDialog(
      title = paste("TOPIC :", input$select_button_ann),
      
      sliderInput("select_lim_top_termes_ann", label = "Limite du nombre de top termes à surligner", min = 1, 
                  max = 100, value = 100),  
      
      checkboxGroupInput("select_top_terms_to_highlight", "Top termes à surligner :",
                   choices = top_terms, selected = selected_top_terms),
      easyClose = TRUE
    ))
    
    dt_ann$start_popup1 <- TRUE
    dt_ann$start_popup2 <- 100
    dt_ann$selected_button_ann <- input$select_button_ann
  }
  
  session$sendCustomMessage(type = 'resetInputValue', message =  "select_button_ann")
  
},priority = 5,ignoreNULL = TRUE,ignoreInit = TRUE)

observeEvent(input$select_top_terms_to_highlight,{
  idx_label <- which(dt_ann$label_df_modele_choix == dt_ann$selected_button_ann)
  dt_ann$highlight_top_terms[[as.character(idx_label)]] <- input$select_top_terms_to_highlight
})

observeEvent(input$select_lim_top_termes_ann,{
  
  if (input$select_lim_top_termes_ann != dt_ann$start_popup2) {
    idx_label <- which(dt_ann$label_df_modele_choix == dt_ann$selected_button_ann)
    columns_names = paste0('X',dt_ann$label_df_modele_choix$cluster)
    df_word_cluster_i = dt_ann$df_word_cluster_tf_modele[,c('terms',columns_names[idx_label])]
    df_word_cluster_i = df_word_cluster_i[order(-df_word_cluster_i[,columns_names[idx_label]]),]
    top_terms = df_word_cluster_i$terms[1:input$select_lim_top_termes_ann]
    
    #top_terms = top_terms[top_terms %in% input$select_top_terms_to_highlight]
    dt_ann$start_popup2 <- input$select_lim_top_termes_ann
    
    updateCheckboxGroupInput(session, "select_top_terms_to_highlight", selected = top_terms)
  }
})


observeEvent(input$bouton_white_ann, {
  
  highlight_text <- input$mydata
  
  # remove space at the start and end
  highlight_text <- stringr::str_trim(highlight_text, "both")
  
  # remove space/punctuation at the start and end
  highlight_text_without_punctuation <- gsub('[[:punct:] ]+',' ',highlight_text)
  highlight_text_without_punctuation <- stringr::str_trim(highlight_text_without_punctuation, "both")
  
  if (nchar(highlight_text) > 0) {
    
    # Remove highlight_text for each topic:
    for (idx in names(dt_ann$list_highlights)) {
      dt_ann$list_highlights[[idx]] <- dt_ann$list_highlights[[idx]][!(dt_ann$list_highlights[[idx]] %in% c(highlight_text, highlight_text_without_punctuation))]
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


observeEvent(c(input$select_max_topic_ann, input$boutons_label, dt_ann$highlight_top_terms),{
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


observeEvent(input$bouton_save_all_ann,{
  
  l <- button_topic(100, dt_ann$label_df_modele_choix, dt_ann$df_document_vector_modele_sample, 
                    dt_ann$df_word_cluster_tf_modele, dt_ann$list_highlights, dt_ann$no_highlights, TRUE,
                    max_example = nrow(dt_ann$df_document_vector_modele_sample))
  
  columns_names = paste0('X',dt_ann$label_df_modele_choix$cluster)
  
  write.csv(dt_ann$df_document_vector_modele[,c("body_as_text", "annotated", columns_names)], file = paste0(path, 'annotated_dataset.csv'))
  
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
  message <- annotated_examples(100, dt_ann$label_df_modele_choix, dt_ann$df_annotated_modele_sample)
  
  output$example_documents_ann <- renderText({message})
  
},priority = 0,ignoreNULL = FALSE,ignoreInit = FALSE)