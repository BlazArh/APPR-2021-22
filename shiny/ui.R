library(shiny)

shinyUI(fluidPage( 
  title = " ", 
  
  sidebarLayout( 
    sidebarPanel( 
      uiOutput("izbor_stolpcev"), 
      uiOutput("izbor_drzav") 
    ), 
    mainPanel( 
      tabsetPanel( 
        id = 'dataset', 
        tabPanel(value="tab_skupna",title = "Skupna", DT::dataTableOutput("tabelaSkupna"))
        ) 
      ) 
      
      
    )))
