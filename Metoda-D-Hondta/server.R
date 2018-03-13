library(shiny)

shinyServer(function(input, output) {
  output$dynamicselect <- renderUI({
    selectInput("select1", "Wybierz partie",
                choices = 1:input$slide1)
  })
  
  v <- reactiveValues(data = 0)
  
  observeEvent(input$act, {
    length(v$data) <- as.numeric(input$slide1)
    v$data[as.numeric(input$select1)] <- as.numeric(input$text1)
  })
  
  a <- reactive({
    a <- cbind("patria" = 1:input$slide1,
               "sondaż" = v$data)
  })
  
  b <- reactive({
    b <- cbind("patria" = 1:input$slide1,
          "sondaż" = v$data)
    b[,1] <- as.character(b[,1])
    b
  })
  
  output$table1 <- renderTable({b()})
  output$table2 <- renderTable({b()})
  output$table3 <- renderTable({b()})
  
  summary_votes <- reactive({
    if(sum(a()[,2], na.rm = TRUE)>1)
    {
      x <- paste0(100 * sum(a()[,2], na.rm = TRUE), "%"
                  , " Suma sondaży nie może przekraczać 100%")
    }
    else
    {
      x <- paste0(100 * sum(a()[,2], na.rm = TRUE), "%" 
                  , " procent głosów. Do rozdysponowania między partie: "
                  , 100 * (1 - sum(a()[,2], na.rm = TRUE)), "%")
    }
  })
  
  output$sumvotes <- renderText({summary_votes()})
  output$sumvotes2 <- renderText({summary_votes()})
  output$sumvotes3 <- renderText({summary_votes()})
  
  dfma <- reactive({
    sounding <- v$data
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
    sounding[boolvec == F] <- 0#odrzucamy <0.05
    sounding <- v$data
    sounding <- sapply(sounding, function(x) rnorm(1, x, 0.02))#uwzględniamy błąd
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
    sounding[boolvec2 == F] <- 0
    #sounding <- c(sounding, 1 - sum(sounding))
    sounding2 <- sounding
    if((1 - sum(sounding2)) > 0)
    {
      sum1 <- sum(sounding2[1:length(sounding2)])
      vec1 <- sounding2[1:length(sounding2)]/sum1
      x <- vec1 * (1 - sum(sounding2))
      sounding2 <- sounding2[1:length(sounding2)] + x
    }
    else
    {
      sum1 <- sum(sounding2[1:length(sounding2)])
      vec1 <- sounding2[1:length(sounding2)]/sum1
      x <- vec1 * (1 - sum(sounding2))
      sounding2 <- sounding2[1:length(sounding2)] + x
    }
    sounding2 <- rep((sounding2 * 10000), times = 460)
    mat <- matrix(sounding2, ncol = input$slide1, byrow = TRUE)
    dfma <- as.data.frame(mat)
    return(dfma)#ramka danych
  })
  
  votes_table <- reactive({
    divvec <- 1:460
    dfma <- dfma()/divvec
    o <- order(dfma, decreasing = TRUE)[1:460]
    inv <- c(1:input$slide1*460)
    tt <- findInterval(o, inv) + 1
    #return(data.frame(sounding2, sounding[1:length(sounding)], 1-sum(sounding)))
    tt <- table(tt)
    tt <- as.data.frame(tt)
    colnames(tt) <- c("Patria", "Liczba głosów")
    if(sum(v$data) < 1 | sum(v$data) == 1)
    {
      return(tt)
    }
    else
    {
      return(data.frame("ERROR"))
    }
  })
  
  votes_table2 <- reactive({
    divvec <- seq(1, 460*2, by = 2)
    dfma <- dfma()/divvec
    o <- order(dfma, decreasing = TRUE)[1:460]
    inv <- c(1:input$slide1*460)
    tt <- findInterval(o, inv) + 1
    #return(data.frame(sounding2, sounding[1:length(sounding)], 1-sum(sounding)))
    tt <- table(tt)
    tt <- as.data.frame(tt)
    colnames(tt) <- c("Patria", "Liczba głosów")
    if(sum(v$data) < 1 | sum(v$data) == 1)
    {
      return(tt)
    }
    else
    {
      return(data.frame("ERROR"))
    }
  })
  
  
  output$votes <- renderTable({votes_table()})
  
  output$votes2 <- renderTable({votes_table2()})
  
  output$votes3 <- renderTable({votes_table()})
  
  output$votes4 <- renderTable({votes_table2()})
  
  
  
  
  
  
  
  
})