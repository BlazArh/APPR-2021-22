library(shiny)
library(reshape2)
library(data.table)
library(DT)

SkupnaTabela$Country <- factor(SkupnaTabela$Country,levels=SkupnaTabela %>% arrange(Dohodek.2019) %>% .$Country, 
                      ordered = TRUE) 

shinyServer(function(input, output) { 
  
  output$izbor_stolpcev = renderUI({ 
    conditionalPanel( 
      'input.dataset === "tab_skupna"', 
      checkboxGroupInput("show_vars", "Kateri stolpec naj prika?e:",
                         names(SkupnaTabela), selected = names(SkupnaTabela)) 
    ) 
  }) 
  output$izbor_drzav = renderUI({ 
    conditionalPanel( 
      'input.dataset === "tab_place"', 
      checkboxGroupInput("izbrane_drzave", "Izberi drzave:", 
                         TabelaPlace$Drzava, selected = c("Slovenia")) 
    ) 
  }) 
  
  
  output$tabelaSkupna <- DT::renderDataTable({DT::datatable(SkupnaTabela %>% select(input$show_vars))}) 
})

