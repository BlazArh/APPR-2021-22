library(shiny)
library(reshape2)
library(data.table)
library(DT)

shinyServer(function(input, output) { 
  
  output$graf <- renderPlot({
    drzava <- input$drzava
    spodnja = input$leto[1]
    zgornja = input$leto[2]
    tip_podatka = input$tip
    data <- skupna %>% filter(Drzava == drzava & Leto <= zgornja & Leto >= spodnja & tip == tip_podatka)
    ggplot(data) + geom_line(aes(x = Leto, y = vrednost))
  })
})
