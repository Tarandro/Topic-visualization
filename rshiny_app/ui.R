dashboardPage(
  dashboardHeader(title = "Topic Modeling"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Thèmes", tabName = "topics"),
      menuItem("Agrégation", tabName = "agregation"),
      menuItem("Hierarchy", tabName = "hierarchy"),
      menuItem("Visualisation", tabName = "visualisation"),
      menuItem("Classification", tabName = "classification"),
      menuItem("Dataset", tabName = "dataset")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "topics",
              source("./src/ui/ui_topics.R", encoding = "UTF-8")$value),
      
      tabItem(tabName = "agregation",
              source("./src/ui/ui_agregation.R", encoding = "UTF-8")$value),
      
      tabItem(tabName = "hierarchy",
              source("./src/ui/ui_hierarchy.R", encoding = "UTF-8")$value),
      
      tabItem(tabName = "visualisation",
              source("./src/ui/ui_visualisation.R", encoding = "UTF-8")$value),
      
      tabItem(tabName = "classification",
              source("./src/ui/ui_classification.R", encoding = "UTF-8")$value),
      
      tabItem(tabName = "dataset",
              source("./src/ui/ui_dataset.R", encoding = "UTF-8")$value)
      
    )
  )
)
