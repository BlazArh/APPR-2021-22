
library(shiny)

shinyUI(fluidPage( 
  title = " ", 
  
  sidebarLayout( 
    sidebarPanel( 
      selectInput('drzava','Izberi državo:', unique(skupna$Drzava)),
      sliderInput( "leto", "Izberi leta:", min = min(skupna$Leto), 
                   max = max(skupna$Leto), step = 1, value = c(2013, 2019)),
      selectInput('tip','Izberi tip podatka:', unique(skupna$tip))
    ), 
    mainPanel( 
      tabsetPanel( 
        id = 'dataset', 
        tabPanel("Graf", 
                 plotOutput("graf")
        )
      ) 
    ) 
    
    
  )))



