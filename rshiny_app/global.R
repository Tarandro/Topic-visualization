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


path = '/home/gassmann/Documents/NLP-TopicModeling/data/CDP_hierar/'  # Tripadvisor_pos

df_label = read.csv(paste0(path,'df_label','.csv'))
top_topic_terms = read.csv(paste0(path,'top_topic_terms','.csv'))
df_document_vector_before = read.csv(paste0(path,'df_document_vector_before','.csv'))
df_document_vector = read.csv(paste0(path,'df_document_vector','.csv'))
df_informations = read.csv(paste0(path,'df_informations','.csv'))
df_info_classif = read.csv(paste0(path,'df_info_classif','.csv'))
df_sim_terme_topic = read.csv(paste0(path,'df_sim_terme_topic','.csv'))
df_sim_doc_topic = read.csv(paste0(path,'df_sim_doc_topic','.csv'))
df_word_cluster_tf = read.csv(paste0(path,'df_word_cluster_tf','.csv'))
df_word_cluster_tfidf = read.csv(paste0(path,'df_word_cluster_tfidf','.csv'))



readUrl <- function(name) {
  out <- tryCatch(
    {read.csv(paste0(path,'matrice_sim_topics_',name,'.csv'))},
    warning=function(cond) {return(NULL)}
  )    
  return(out)
}

name_dataset = c('df_document_vector_before','df_document_vector','top_topic_terms','df_info_classif','df_sim_terme_topic',
                 'df_sim_doc_topic','df_informations','df_word_cluster_tf','df_word_cluster_tfidf','df_label')

name_modeles = c()

for(modele in unique(df_label$modele)){
  name_modeles = c(name_modeles, modele)
}

# create a reactiveValues with all datasets
datas = reactiveValues()
for(name in name_dataset){
  datas[[name]] = read.csv(paste0(path,name,'.csv'))
}

for(modele in name_modeles){
  datas[[paste0('matrice_sim_topics_',modele)]] = readUrl(modele)
}

reticulate::use_python('/home/gassmann/anaconda3/bin/python3.8', required = T)
library(reticulate)
py_config()

source_python('/home/gassmann/PycharmProjects/NMF-script/main.py')
#fasttext = ft_document_vector(subset(df_document_vector, modele==df_document_vector$modele[1])$terms, 1L, 200L, 5L)
