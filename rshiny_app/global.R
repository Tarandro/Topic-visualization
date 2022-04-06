## packages -----
require(shiny)
require(shinydashboard)
require(shinyWidgets)
require(data.table)
require(magrittr)
require(DT)
require(plotly)
require(htmlwidgets)
require(reshape)
require(viridis)
require(sqldf)
require(visNetwork)
require(geomnet)
require(igraph)
require(dplyr)
require(stringr)
require(ramify)
require(rsconnect)


#library(rsconnect)
#deployApp("~/Documents/NLP-TopicModeling/Topic-visualization/rshiny_app")

path = "CDP_hierar/"  # Tripadvisor_pos
path = "C:/Users/AlexisGassmann/Documents/cdp_dataset/"


df_label = read.csv(paste0(path,'df_label','.csv'))
df_label$label <- mapply(function(x){return(strsplit(x, "_")[[1]][1])}, df_label$label)
df_label <- df_label[!((duplicated(df_label[, c("label", "cluster", "modele")]) | duplicated (df_label[, c("label", "cluster", "modele")], fromLast = TRUE)) & df_label$choix == 0),]

top_topic_terms = read.csv(paste0(path,'top_topic_terms','.csv'))
df_hierarchy_top_terms = read.csv(paste0(path,'df_hierarchy_top_terms','.csv'))
df_document_vector_before = read.csv(paste0(path,'df_document_vector_before','.csv'))
df_document_vector = read.csv(paste0(path,'df_document_vector','.csv'))
df_informations = read.csv(paste0(path,'df_informations','.csv'))
df_info_classif = read.csv(paste0(path,'df_info_classif','.csv'))
df_sim_terme_topic = read.csv(paste0(path,'df_sim_terme_topic','.csv'))
df_sim_doc_topic = read.csv(paste0(path,'df_sim_doc_topic','.csv'))
df_word_cluster_tf = read.csv(paste0(path,'df_word_cluster_tf','.csv'))
df_word_cluster_tfidf = read.csv(paste0(path,'df_word_cluster_tfidf','.csv'))
df_lemma = read.csv(paste0(path,'df_lemma','.csv'))


readUrl <- function(name) {
  out <- tryCatch(
    {read.csv(paste0(path,'matrice_sim_topics_',name,'.csv'))},
    warning=function(cond) {return(NULL)}
  )    
  return(out)
}

name_dataset = c('df_document_vector_before','df_document_vector','top_topic_terms','df_hierarchy_top_terms','df_info_classif','df_sim_terme_topic',
                 'df_sim_doc_topic','df_informations','df_word_cluster_tf','df_word_cluster_tfidf','df_lemma','df_label',
                 'dataset_preprocessed')

name_modeles = c()

for(modele in unique(df_label$modele)){
  name_modeles = c(name_modeles, modele)
}

# create a reactiveValues with all datasets
datas = reactiveValues()
for(name in name_dataset){
  if (name == "df_label") {
    dt <- read.csv(paste0(path,name,'.csv'))
    dt$label <- mapply(function(x){return(strsplit(x, "_")[[1]][1])}, dt$label)
    dt <- dt[!((duplicated(dt[, c("label", "cluster", "modele")]) | duplicated (dt[, c("label", "cluster", "modele")], fromLast = TRUE)) & dt$choix == 0),]
    datas[[name]] <- dt
  }
  else if (name == "df_document_vector") {
    dt <- read.csv(paste0(path,name,'.csv'))
    dt[["annotated"]] <- ""
    datas[[name]] <- dt
  }
  else{
    datas[[name]] <- read.csv(paste0(path,name,'.csv'))
  }
}

for(modele in name_modeles){
  datas[[paste0('matrice_sim_topics_',modele)]] = readUrl(modele)
}

#reticulate::use_python('/home/gassmann/anaconda3/bin/python3.8', required = T)
#library(reticulate)
#py_config()

#source_python('/home/gassmann/PycharmProjects/NMF-script/main.py')
#fasttext = ft_document_vector(subset(df_document_vector, modele==df_document_vector$modele[1])$terms, 1L, 200L, 5L)
