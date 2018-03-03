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
  
  output$sumvotes <- renderText({
      if(sum(a()[,2])>1)
      {
        x <- paste0(100 * sum(a()[,2]), "%"
                    , " Suma sondaży nie może przekraczać 100%")
      }
      else
      {
        x <- paste0(100 * sum(a()[,2]), "%" 
                    , " procent głosów. Do rozdysponowania między partie: "
                    , 100 * (1 - sum(a()[,2])), "%")
      }
    })
  
  output$votes <- renderTable({
  boolvec <- sapply(v$data, function(x){# true false aby odrzucić 
    if(x<0.05)#sondaże za małe
    {
      y <- FALSE
     }
      else
      {
         y <- TRUE
       }
     })
     sounding <- v$data[boolvec]#odrzucamy <0.05
     sounding <- sapply(sounding, function(x) rnorm(1, x, 0.1*x))#uwzględniamy błąd
     boolvec2 <- sapply(sounding, function(x){# true false aby odrzucić 
      if(x<0.05)#sondaże za małe
      {
         y <- FALSE
      }
        else
       {
                y <- TRUE
     }
     })
    sounding <- sounding[boolvec]
    
    sounding <- rep(sounding * input$votes_count, times = 460)
    mat <- matrix(sounding, ncol = input$slide1, byrow = TRUE)
    dfma <- as.data.frame(mat)
    divvec <- 1:460
    dfma <- dfma/divvec
    o <- order(dfma, decreasing = TRUE)[1:460]
    inv <- c(1:input$slide1*460)
    tt <- findInterval(o, inv)
    return(table(tt))
  })
})





