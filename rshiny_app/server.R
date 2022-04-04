shinyServer(function(input, output, session) {
  # differents onglets :
  source("src/server/server_introduction.R", local = TRUE, encoding = "UTF-8")
  
  source("src/server/server_topics.R", local = TRUE, encoding = "UTF-8")
  
  source("src/server/server_agregation.R", local = TRUE, encoding = "UTF-8")
  
  source("src/server/server_hierarchy.R", local = TRUE, encoding = "UTF-8")
  
  source("src/server/server_documents.R", local = TRUE, encoding = "UTF-8")
  
  source("src/server/server_visualisation.R", local = TRUE, encoding = "UTF-8")
  
  source("src/server/server_dataset.R", local = TRUE, encoding = "UTF-8")
  
  source("src/server/server_annotation.R", local = TRUE, encoding = "UTF-8")
  
})
