highlight <- '
                function getSelectionText() {
var text = "";
if (window.getSelection) {
text = window.getSelection().toString();
} else if (document.selection) {
text = document.selection.createRange().text;
}
return text;
}

document.onmouseup = document.onkeyup = document.onselectionchange = function() {
var selection = getSelectionText();
Shiny.onInputChange("mydata", selection);
};
'

tabItem(
  tabName = "annotation",
  
  tags$script(highlight),
  
  tabsetPanel(
    id = "tabs_annotation",
    tabPanel("Annotate",
  
    fluidRow(
      tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: purple}")),
      column(2,
             pickerInput("Choix_modele_ann", "Modèle", as.vector(unique(top_topic_terms$modele)),selected = as.vector(unique(top_topic_terms$modele))[1])
      ),
      column(1),
      column(3,
             uiOutput("ui_max_topic")       
      ),
      column(2),
      column(2,
             actionButton("bouton_rerun_ann", "Autres exemples", style='height:60px')
             , style = "margin-top: 30px;"
      ),
      column(2,
             actionButton("bouton_save_all_ann", "Tout sauvegarder", style='height:60px')
             , style = "margin-top: 30px;"
      ),
    ),
    
    fluidRow(
      tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: purple}")),
      column(5,
             h3(htmlOutput("html_button_topic"))
      ),
      column(7,
             h3(htmlOutput("html_show_text")),
             br(),
             fluidRow(
               column(2, actionButton("bouton_white_ann", "Retirer",
                                      style="text-align:center;width: 100%;background-color:#FFFFFF;color:black;border-radius: 5px;border: 2px solid #000000")),
               column(3),
               column(2, actionButton("bouton_save_ann", "Sauvegarder", style='height:60px'))
             )
             
      )
    )
    ),
    tabPanel("Annotate",
             fluidRow(column(12, actionButton("bouton_rerun_ann_ex", "Autres exemples", style='height:60px'), align = "center")),
             h3(htmlOutput("example_documents_ann"))
    )
  )
)