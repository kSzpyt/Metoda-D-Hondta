library(shiny)

shinyUI(fluidPage(
  
  
  titlePanel("Metoda D'Hondta"),
  
  
  sidebarLayout(
    sidebarPanel("Wybierz liczbę partii",
                 sliderInput("slide1", "Wybierz liczbę partii",
                             2, 10, 3),
                 uiOutput("dynamicselect"),
                 textInput("text1", "wprowadź sondaż", 0.37),
                 actionButton("act", "zaktualizuj")
    ),
    
    
    mainPanel("ss",
              tableOutput("table1")
              
    )
  )
))
