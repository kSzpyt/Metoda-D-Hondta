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
    
    mainPanel(
      tabsetPanel(type="pills",
                  tabPanel("Metoda D'Hondta",
                           tableOutput("table1"),
                           textOutput("sumvotes"),
                           tableOutput("votes")
                           ),
                  tabPanel("Metoda Sainte-Laguë",
                           tableOutput("table2"),
                           textOutput("sumvotes2"),
                           tableOutput("votes2")
                           ),
                  tabPanel("Porównanie", 
                           tableOutput("table3"),
                           textOutput("sumvotes3"),
                           h4("Metoda D'Hondta"),
                           tableOutput("votes3"),
                           h4("Metoda Sainte-Laguë"),
                           tableOutput("votes4")
                           )
                  )
              
              
    )
  )
))
