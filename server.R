library(shiny)

shinyServer(function(input, output){
  
  source("initiate.R")
  
  events <- load.events()
  showers <- load.showers()
  
  data <- reactiveValues(shower = NULL, shower.events = NULL, radiants = NULL,
                         n.events = NULL, n.outliers = NULL)
  
  observeEvent(input$generate,{
    data$shower <- find.shower(input$name, input$year, showers)
    data$shower.events <- find.events(data$shower$start.date,
                                      data$shower$end.date, 
                                      events)
    data$radiants <- shower.radiant(data$shower.events)
    
    data$n.events <- length(data$shower.events)
    
    if(input$remove.outliers){
      data$shower.events <- outlier.trim(data$shower.events,
                                         data$radiants,
                                         nbins = input$nbins,
                                         multiplier = input$iqr)
      
      data$radiants <- shower.radiant(data$shower.events)
      
      data$n.outliers <- data$n.events - length(data$shower.events)
    }
  })
  
  output$radiant.plot <- renderPlot({
    if(is.null(data$shower) || is.null(data$shower.events) 
       || is.null(data$radiants)) return()
    shower <- data$shower
    shower.events <- data$shower.events
    radiants <- data$radiants
    
    ggplot(radiants, aes(radiants$ra, radiants$dec)) + 
      stat_bin2d(bins = input$nbins) + 
      ggtitle(paste(substr(shower$start.date, 1, 4), " ", shower$name, 
                    " (", shower$abbrev, ") ", sep = "")) + 
      labs(x = "Radiant (degrees)", y = "Declination (Degrees)")
  })
  
  output$shower.info <- renderTable(
    if(is.null(data$shower) || is.null(data$radiants)){
      return()
    } 
    else{
      radiant.hist <- hist2d(data$radiants, nbins = input$nbins, show = FALSE)
      cbind(data$shower[c("name", "abbrev", "start.date", "end.date", "peak.date", 
                  "theo.ra", "theo.dec")], mode2d(radiant.hist))
    }
  )
  
  output$event.info <- renderText(
    if(is.null(data$n.events)){
      return()
    }
    else{
      paste("Number of events:", data$n.events)
    }
  )
  
  output$outlier.info <- renderText(
    if(!input$remove.outliers || is.null(data$n.outliers)){
      return()
    }
    else{
      paste("Number of outliers:", data$n.outliers)
    }
  )
  
  
  #Mode, number of outliers, shower info
})