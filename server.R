library(shiny)

shinyServer(function(input, output){
  
  library(astrolibR)
  library(stringr)
  library(gplots)
  library(ggplot2)
  library(shiny)
  library(MASS)
  
  source("event.R")
  source("import.R")
  source("radiant.R")
  source("shower.R")
  source("outlier.R")
  
  events <- load.events()
  showers <- load.showers()
  
  data <- reactiveValues(shower = NULL, shower.events = NULL, radiants = NULL,
                         n.events = NULL, n.outliers = NULL)

  
  observeEvent(input$generate,{
    print(str_match(input$name, ".*(?= \\()"))
    data$shower <- find.shower(str_match(input$name, "[A-Z]{3}(?=\\))")[[1]], input$year, showers)
    print(input$name)
    print(str_match(input$name, "[A-Z]{3}(?=\\))")[[1]])
    print(input$year)
    print(data$shower)
    print(identical(data$shower$name, character(0)))
    if(identical(data$shower$name, character(0))){
      data$n.events <- 0
      return()
    }
    
    data$shower.events <- find.events(data$shower$start.date,
                                      data$shower$end.date, 
                                      events)
    data$n.events <- length(data$shower.events)
    
    if(file.exists(paste("./save-files/radiants/", data$shower$abbrev, 
                         substr(data$shower$start.date, 1, 4), ".txt", sep=""))){
      data$radiants <- load.radiant(data$shower)
    } else{
      data$radiants <- shower.radiant(data$shower.events)
    }
    
    if(input$remove.outlier.intersections){
      #data$shower.events <- outlier.trim(data$shower.events,
      #                                  data$radiants,
      #                                  nbins = input$nbins,
      #                                  multiplier = input$iqr)
      
      #data$radiants <- shower.radiant(data$shower.events)
      
      #data$n.outliers <- data$n.events - length(data$shower.events)
      data$n.outlier.intersections <- nrow(data$radiants[data$radiants$antirad < 0,])
      data$radiants <- data$radiants[data$radiants$antirad >= 0,]
      
    } else{
      data$n.outlier.intersections <- 0
    }
    data$n.intersections <- nrow(data$radiants)
  })
  
  output$radiant.plot <- renderPlot({
    if(is.null(data$shower) || is.null(data$shower.events) 
       || is.null(data$radiants) || data$n.events < 2) {
      return()
    }
    
    shower <- data$shower
    shower.events <- data$shower.events
    radiants <- data$radiants
    
    ggplot(radiants, aes(radiants$ra, radiants$dec)) + 
      stat_bin2d(bins = input$nbins) + 
      ggtitle(paste(substr(shower$start.date, 1, 4), " ", shower$name, 
                    " (", shower$number, " ", shower$abbrev, ") ", sep = "")) + 
      labs(x = "Right Ascension (Degrees)", y = "Declination (Degrees)")
  })
  
  #output$ra.plot <- renderPlot({
  #  if(is.null(data$shower) || is.null(data$shower.events) 
  #     || is.null(data$radiants) || data$n.events < 2) {
  #    return()
  #  }
  #  shower <- data$shower
  #  shower.events <- data$shower.events
  #  radiants <- data$radiants
  #  
  #  ggplot(radiants$ra, aes)
  #  
  #})
  
  output$shower.info <- renderTable(
    
    if(is.null(data$shower) || is.null(data$radiants) || data$n.events < 3){
      return()
    } 
    else{
      radiants <- data$radiants
      radiant.hist <- hist2d(x=radiants$ra, y=radiants$dec, nbins = input$nbins, show = FALSE)
      c <- 1 / data$n.events * sum(cos(radiants$ra * pi / 180))
      s <- 1 / data$n.events * sum(sin(radiants$ra * pi / 180))
      
      if(c < 0){
        mean.ra <- (atan(s / c) + pi) * 180 / pi
      }
      else{
        mean.ra <- atan(s / c) * 180 / pi
      }
      
      mean.dec <- mean(radiants$dec)
      
      cbind(data$shower[c("name", "number", "abbrev", "start.date", "end.date", "peak.date", 
                  "theo.ra", "theo.dec")], mean.ra, mean.dec, mode2d(radiant.hist))
    }
  )
  
  output$event.info <- renderText(
    if(is.null(data$n.events)){
      return()
    }
    else{
      #n.intersections <- as.numeric(data$n.events * data$n.events - data$n.events)/2
      paste(data$shower["name"], " radiant computation complete.          
            Number of events: ", data$n.events, "Number of intersects: ", 
            data$n.intersections)
    }
  )
  
  output$outlier.info <- renderText(
    if(!input$remove.outlier.intersections){
      return()
    }
    else{
      paste("Number of outlier intersections:", data$n.outlier.intersections)
    }
  )
  
  
  #Mode, number of outliers, shower info
})