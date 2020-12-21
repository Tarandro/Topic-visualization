dt_hierarchy = reactiveValues(label_df_modele_choix = NULL, top_topic_terms_modele = NULL, df_hierarchy_top_terms_modele = NULL)

# udpate si on change de label pour un topic
observeEvent({input$Choix_modele_hier
  input$boutons_label
  input$change_topic}, {
    dt_hierarchy$label_df_modele_choix = subset(dt_topics$label_df_choix, modele == input$Choix_modele_hier)
    dt_hierarchy$top_topic_terms_modele = subset(datas$top_topic_terms, modele == input$Choix_modele_hier)
    dt_hierarchy$df_hierarchy_top_terms_modele = subset(datas$df_hierarchy_top_terms, modele == input$Choix_modele_hier)
  },priority = 2,ignoreNULL = FALSE,ignoreInit = FALSE)

#######################################################################################"

get_top_terms = function(string_list, n){
  " design the top terms presentation from a string_list of top terms "
  string_list = gsub("'", "",string_list)
  string_list = substring(string_list, 2)
  string_list = strsplit(string_list,', ')
  string_list = string_list[[1]]
  
  message = ""
  for (i in 1:n) {
    if (i == 1) {
      message = paste(message,string_list[i])
    }
    else{
      message = paste(message, '-',string_list[i])
    }
    
  }
  message = substring(message, 2)
  return(message)
}

viznetwork= function(label_df, df_top_terms, df_hierarchy){
    " crée les dataframes nodes et edges puis visualisation avec vizNetwork "
    title = c()
    for (topic in label_df$cluster) {
      message = ""
      for (i in 1:5) {
        message = paste(message, '-',subset(df_top_terms, cluster == topic)$terms[i])
      }
      title = c(title,message)
    }
    
    ######
    nodes = as.data.frame(label_df$label)
    colnames(nodes) = c("label")
    nodes$id = nodes$label
    nodes$group = label_df$cluster
    nodes$title = title
    center = c('center','center',-1,'center')
    nodes = rbind(nodes, center)
    
    edges <- as.data.frame(label_df$label)
    colnames(edges) = c("to")
    edges$from = 'center'
    edges$width = '2'
    
    ######
    # si children topics :
    if (length(df_hierarchy$top_terms)>0) {
        copy_df_hierarchy = copy(df_hierarchy)
        copy_df_hierarchy$top_3 = unlist(lapply(copy_df_hierarchy$top_terms, get_top_terms, n=3))
        #can have duplicate of top 3 terms:
        copy_df_hierarchy = copy_df_hierarchy[!duplicated(copy_df_hierarchy$top_3), ]
        copy_df_hierarchy$top_5 = unlist(lapply(copy_df_hierarchy$top_terms, get_top_terms, n=5))
        
        nodes_child = as.data.frame(copy_df_hierarchy$top_3)
        colnames(nodes_child) = c("label")
        nodes_child$id = nodes_child$label
        nodes_child$group = copy_df_hierarchy$cluster
        nodes_child$title = copy_df_hierarchy$top_5
        
        nodes = rbind(nodes, nodes_child)
        
        edges_child <- as.data.frame(copy_df_hierarchy$top_3)
        colnames(edges_child) = c("to")
        b = merge(copy_df_hierarchy, label_df, by = 'cluster')
        edges_child$from = b$label
        edges_child$width = '1'
        
        edges = rbind(edges, edges_child)
    }
    ######
    network = visNetwork(nodes, edges, width = "100%") %>%
      visIgraphLayout() %>%
      visNodes(
        shape = "dot",
        color = list(
          background = "#0085AF",
          border = "#013848",
          highlight = "#FF8000"
        ),
        shadow = list(enabled = TRUE, size = 10)
      ) %>%
      visEdges(
        shadow = FALSE,
        color = list(color = "#0085AF", highlight = "#C62F4B")
      ) %>%
      visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T)) %>% 
      visLayout(randomSeed = 11)
    
    return(network)
}


output$network <- renderVisNetwork({
  viznetwork(dt_hierarchy$label_df_modele_choix, dt_hierarchy$top_topic_terms_modele, dt_hierarchy$df_hierarchy_top_terms_modele)
})