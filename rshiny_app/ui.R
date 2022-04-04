dashboardPage(
  dashboardHeader(title = "Topic Modeling"),
  
  dashboardSidebar(
    sidebarMenu(
      id = 'tabs',
      menuItem("Introduction", tabName = "introduction"),
      menuItem("Thèmes", tabName = "topics"),
      menuItem("Agrégation", tabName = "agregation"),
      menuItem("Hierarchy", tabName = "hierarchy"),
      menuItem("Documents", tabName = "documents"),
      menuItem("Visualisation", tabName = "visualisation"),
      menuItem("Dataset", tabName = "dataset"),
      menuItem("Annotation", tabName = "annotation")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "introduction",
              source("./src/ui/ui_introduction.R", encoding = "UTF-8")$value),
      
      tabItem(tabName = "topics",
              source("./src/ui/ui_topics.R", encoding = "UTF-8")$value),
      
      tabItem(tabName = "agregation",
              source("./src/ui/ui_agregation.R", encoding = "UTF-8")$value),
      
      tabItem(tabName = "hierarchy",
              source("./src/ui/ui_hierarchy.R", encoding = "UTF-8")$value),
      
      tabItem(tabName = "documents",
              source("./src/ui/ui_documents.R", encoding = "UTF-8")$value),
      
      tabItem(tabName = "visualisation",
              source("./src/ui/ui_visualisation.R", encoding = "UTF-8")$value),
      
      tabItem(tabName = "dataset",
              source("./src/ui/ui_dataset.R", encoding = "UTF-8")$value),
      
      tabItem(tabName = "annotation",
              source("./src/ui/ui_annotation.R", encoding = "UTF-8")$value)
      
    )
  )
)
