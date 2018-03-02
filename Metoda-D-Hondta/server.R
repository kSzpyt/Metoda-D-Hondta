library(shiny)

shinyServer(function(input, output) {
  
  
  output$dynamicselect <- renderUI({
    
    selectInput("select1", "Wybierz partie",
                choices = 1:input$slide1)
  })
  
  
  v <- reactiveValues(data = NULL)
  
  observeEvent(input$act, {
    length(v$data) <- as.numeric(input$slide1)
    v$data[as.numeric(input$select1)] <- as.numeric(input$text1)
  })
  
  a <- reactive({
    a <- cbind("patria" = 1:input$slide1,
               "sondaż" = v$data)
  })
  
  output$table1 <- renderTable({
    a()
    
    
  })
  
  
})





