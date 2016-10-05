

library(shiny)
library(shinyjs)
library(cluster)
# source("functions.R")
options(shiny.maxRequestSize = 9 * 1024 ^ 2)
#source("server_functions.R")

shinyServer(function(input, output, session) {
  # INPUT
  
  
  
  observe({
    if (input$submit > 0) {
      shinyjs::toggle(id = "enter", anim = TRUE)
    }
  })
  output$myFileUI <- renderUI({
    input$clear
    input$uploadFormat
    fileInput(
      'file1',
      ' ',
      accept = c(
        'text/csv',
        'text/comma-separated-values',
        'text/tab-separated-values',
        'text/plain',
        '.csv',
        '.tsv'
      )
    )
  })
  test_data <- reactive({
    #print(input$file1)
    if (input$tabs == "demo") {
      inFileString <- input$selectDemoData
      if (inFileString == "")
        return(NULL)
      test_data <- eval(as.symbol(inFileString))
    }
    
    
    else {
      if (input$tabs == "from_computer") {
        inFile <- input$file1
        if (is.null(inFile))
          return(NULL)
        test_data <- read.csv(
          inFile$datapath,
          header = input$header,
          sep = input$sep,
          dec = input$dec,
          quote = input$quote
        )
      } else {
        if (input$tabs == "model_data"){
          lenght <- input$model_data_lenght
          mean <- as.numeric(as.character(input$model_data_mean))
          sigma <- as.numeric(as.character(input$model_data_sigma))
          test_data_vector <- rnorm(lenght, mean, sigma)
          #test_data_vector <- rnorm(100)
          
          test_data <- data.frame(test_data_vector)
          print(test_data_vector)
        }
      }
      
    }
    
    
    test_data
    
    
  })
  
  # HANDLING
  
  temp_data <- reactive({
    temp_data <- test_data()
    
    
    if (is.null(input$dependet_pro))
      return(temp_data)
    names_all <- c(input$undependet_pro, input$dependet_pro)
    names_temp_data <- names(temp_data)
    names_off <-
      names_temp_data[names_temp_data %in% names_all == F]
    temp_data[, names(temp_data) %in% names_all == T]
    
  })
  
  temp_vector <- reactive({
    if (is.null(temp_data()))
      return(NULL)
    temp_data <- temp_data()
    nams <- input$undependet_pro
    temp_vector <- temp_data[, nams]
    temp_vector
  })
  
  output$undependent <- renderUI({
    if (is.null(test_data()))
      return(NULL)
    nams <- names(test_data())
    
    radioButtons('undependet_pro', 'Вектор', choices = as.list(nams))
  })
  
  output$hist_plot <- renderPlot({
    if (is.null(temp_data()))
      return(NULL)
    temp_vector <- temp_vector()
    main <- NULL
    nams <- input$undependet_pro
    bins <- input$number_breaks
    df <- data.frame(temp_vector)
    if (input$hist_probability) {
      ggplot(df, aes(temp_vector)) +
        geom_histogram(
          aes(y = ..density..),
          bins = bins,
          fill = "cornflowerblue",
          colour = "gray"
        ) + geom_density(col = "green",
                         lwd = 1,
                         inherit.aes = T)  + ggtitle(main)
    } else{
      ggplot(df, aes(temp_vector)) +
        geom_histogram(bins = bins,
                       fill = "cornflowerblue",
                       colour = "gray") + ggtitle(main)
    }
    
    
    
    
    
    
    
    
  })
  
  output$summary <- renderPrint({
    if (is.null(temp_vector()))
      return(NULL)
    summary(temp_vector())
    
  })
  output$str <- renderPrint({
    if (is.null(temp_vector()))
      return(NULL)
    str(temp_vector())
    
  })
  
  output$hist_ecdf <- renderPlot({
    if (is.null(temp_data()))
      return(NULL)
    temp_vector <- temp_vector()
    #print(temp_data)
    nams <- input$undependet_pro
    #breaks <- input$number_breaks
    
    #print(nams)
    #print(temp_data[, nams])
    df <- data.frame(temp_vector)
    ggplot(df, aes(temp_vector)) +
      stat_ecdf()
    
    
  })
  output$hist_ecdf_custom <- renderPlot({
    if (is.null(temp_data()))
      return(NULL)
    temp_vector <- temp_vector()
    #print(temp_data)
    nams <- input$undependet_pro
    #breaks <- input$number_breaks
    
    #print(nams)
    #print(temp_data[, nams])
    #nn <- seq(1, max(temp_vector), length.out = length(temp_vector))
    df <-
      data.frame(nn = sort(temp_vector), ttt = as.numeric(table(cut((temp_vector), breaks =
                                                       length(temp_vector)
      ))))
    df$ttt_cum <- cumsum(df$ttt)
    df$ttt_cum <- df$ttt_cum / max(df$ttt_cum)
    #print(df)
    d <- sqrt(-0.5 * log(0.05 / 2) / length(temp_vector))
    #print(d)
    plot(df$nn, df$ttt_cum, type = "l", panel.first = grid( lty = 1), lwd = 2, col = "blue")
    df$ttt_cum_d <- df$ttt_cum + d
    df$ttt_cum__d <- df$ttt_cum - d
    #if(df$ttt_cum__d < 0) df$ttt_cum__d = -10
    lines(df$nn[ df$ttt_cum_d <= 1], df$ttt_cum_d[ df$ttt_cum_d <= 1], lty= 2)
    lines(df$nn[ df$ttt_cum__d >= 0], df$ttt_cum__d[ df$ttt_cum__d >= 0], lty= 2)
    #############
    df <- data.frame(t = temp_vector)
    mean <- mean(df$t)
    sigma <- sqrt(var(df$t))
    df$z <- (df$t - mean)/sigma
    df$z <- ifelse(df$z == 0, df$z + 0.00005, df$z)
    
    a <- df$z
    df$Fx_T <- 0.5 - a/(2*abs(a)) + (a/abs(a))/(1+exp(-0.0725*abs(a)*(22 + abs(a)^1.96)))
    df <- df[order(df$t),]
    lines(df$t , df$Fx_T, type = "l", lwd = 2, col = "green")
  })
  output$plot_approcsimation <- renderPlot({
    if (is.null(temp_data()))
      return(NULL)
    temp_vector <- temp_vector()

    df <- data.frame(t = temp_vector)
    mean <- mean(df$t)
    sigma <- sqrt(var(df$t))
    df$z <- (df$t - mean)/sigma
    df$z <- ifelse(df$z == 0, df$z + 0.00005, df$z)
    
    a <- df$z
    df$Fx_T <- 0.5 - a/(2*abs(a)) + (a/abs(a))/(1+exp(-0.0725*abs(a)*(22 + a*a)))
    df <- df[order(df$t),]
    plot(df$t , df$Fx_T, type = "l")
  })
  output$plot_plotnost <- renderPlot({
    if (is.null(temp_data()))
      return(NULL)
    temp_vector <- temp_vector()
    
    df <- data.frame(t = temp_vector)
    mean <- mean(df$t)
    sigma <- sqrt(var(df$t))
    df$z <- (df$t - mean)/sigma
    df$z <- ifelse(df$z == 0, df$z + 0.00005, df$z)
    
    a <- df$z
    df$plotnst <- 1/(sqrt(2*3.1416*sigma*exp(0.5*a*a)))
    df <- df[order(df$t),]
    plot(df$t , df$plotnst, type = "l")
  })
  
})
