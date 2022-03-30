session$onSessionEnded(stopApp)

# sélectionne le bouton rerun
observeEvent({input$bouton_other_examples},{
  
  # take only text with n_char > mean(nchar)
  df_document_sample = datas$dataset_preprocessed[nchar(datas$dataset_preprocessed$body_as_text) > mean(nchar(datas$dataset_preprocessed$body_as_text)),]
  df_document_sample = df_document_sample[sample(1:nrow(df_document_sample)),][1:10,]["body_as_text"]
  
  output$example_dataset <- DT::renderDT({

    colnames(df_document_sample) <- c("Documents")
    
    default_options = list(
      drawCallback = DT::JS("function() {Shiny.bindAll(this.api().table().node());}"),
      searching = F, paging = F, autoWidth = T, info = F, dom = 't',
      width='100%', scrollX = TRUE,
      columnDefs = list(
        list(className =  "dt-center", targets = "_all")
      )
    )
    
    DT::datatable(
      data = df_document_sample,
      colnames = names(df_document_sample),
      rownames = FALSE,
      escape = FALSE,
      selection = "none",
      extensions = 'AutoFill',
      options = default_options
    )
  })
  
},priority = 0,ignoreNULL = FALSE,ignoreInit = FALSE)



