library(shiny)

shinyServer(function(input, output) {
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
  source("fit.R")
  
  events <- load.events()
  showers <- load.showers()
  
  data <-
    reactiveValues(
      shower = NULL,
      shower.events = NULL,
      radiants = NULL,
      n.events = NULL,
      n.outliers = NULL
    )
  
  
  get.shower.info <- function() {
    shower.info <-
      cbind(data$shower[c("name",
                          "number",
                          "abbrev",
                          "start.date",
                          "end.date",
                          "peak.date")])
    return(shower.info)
  }
  
  get.radiant.info <- function() {
    radiants <- data$radiants
    nbins <- input$nbins
    n.events <- data$n.events
    n.outliers <- data$n.outliers
    
    radiant.hist <-
      hist2d(
        x = radiants$ra,
        y = radiants$dec,
        nbins = nbins,
        show = FALSE
      )
    c <- 1 / n.events * sum(cos(radiants$ra * pi / 180))
    s <- 1 / n.events * sum(sin(radiants$ra * pi / 180))
    
    if (c < 0) {
      mean.ra <- (atan(s / c) + pi) * 180 / pi
    }
    else{
      mean.ra <- atan(s / c) * 180 / pi
    }
    
    mean.dec <-
      mean(radiants$dec) #Should probably be adjusted based on histogram mode
    
    
    return(cbind(
      data$shower[c("theo.ra", "theo.dec")],
      mean.ra,
      mean.dec,
      mode2d(radiant.hist),
      n.events,
      n.outliers
    ))
  }
  
  observeEvent(c(input$name, input$year), {
    data$shower <-
      find.shower(str_match(input$name, "[A-Z]{3}(?=\\))")[[1]],
                  input$year,
                  showers)
    print(data$shower)
    if (identical(data$shower$name, character(0))) {
      message("No shower found with name: ",
              input$name,
              " for year",
              input$year)
      data$shower.events <- NULL
      radiants <- NULL
    }
    else{
      data$shower.events <- find.events(data$shower$peak.date,
                                        data$shower$peak.date,
                                        events)
    }
    data$n.events <- length(data$shower.events)
  })
  
  
  observeEvent(c(
    input$name,
    input$year,
    input$aggression,
    input$remove.antiradiants
  ),
  {
    if (file.exists(paste(
      "./save-files/radiants/",
      data$shower$abbrev,
      substr(data$shower$start.date, 1, 4),
      ".txt",
      sep = ""
    ))) {
      data$radiants <- load.radiant(data$shower)
    } else{
      data$radiants <- shower.radiant(data$shower.events)
    }
    
    aggression <- 0.01 * input$aggression
    if (aggression > 0 && data$n.events > 2) {
      suspected.events <- suspect.events(
        data$shower,
        remove.antiradiant = input$remove.antiradiants,
        aggression = aggression
      )
      data$radiants <- remove.event(data$radiants, suspected.events)
      data$n.outliers <- length(suspected.events)
      
    } else{
      data$n.outliers <- 0
    }
    if (is.null(data$radiants))
      data$n.intersections <- 0
    else
      data$n.intersections <- nrow(data$radiants)
  })
  
  
  output$radiant.plot <- renderPlot({
    validate(need(!is.null(data$shower), "Shower not found."))
    validate(need(
      !is.null(data$shower.events),
      "No events found during this shower."
    ))
    validate(need(
      !is.null(data$radiants),
      "Not enough events found during this shower to produce a radiant."
    ))
    validate(
      need(
        data$n.events - data$n.outliers > 2,
        "Too many outliers found. Aggression set too high."
      )
    )
    
    ggplot(data$radiants, aes(data$radiants$ra, data$radiants$dec)) +
      stat_bin2d(bins = input$nbins) +
      ggtitle(
        paste(
          substr(data$shower$start.date, 1, 4),
          " ",
          data$shower$name,
          " (",
          data$shower$number,
          " ",
          data$shower$abbrev,
          ") ",
          sep = ""
        )
      ) +
      labs(x = "Right Ascension (Degrees)", y = "Declination (Degrees)")
  })
  
  
  output$shower.table <- renderTable(if (is.null(data$shower) ||
                                         is.null(data$radiants) ||
                                         (data$n.events - data$n.outliers) < 3) {
    return()
  }
  else{
    get.shower.info()
  })
  
  output$radiant.table <- renderTable(if (is.null(data$shower) ||
                                          is.null(data$radiants) ||
                                          (data$n.events - data$n.outliers) < 3) {
    return()
  }
  else{
    get.radiant.info()
  })
  
  
  output$event.info <- renderText(if (data$n.events > 0)
    paste("Number of events: ", data$n.events))
  
  
  output$intersect.info <- renderText(if (data$n.events > 0)
    paste("Number of intersections: ", data$n.intersections))
  
  
  output$outlier.info <- renderText(if (data$n.events > 0)
    paste("Number of outlier events:", data$n.outliers))
  
  
  observeEvent(input$save, {
    # Shower name of the form XXXYYYYAGGXX,
    #   where xxx is the abbreviation, YYYY is the year
    shower.name <- paste0(data$shower["abbrev"],
                          substr(data$shower["start.date"], 1, 4),
                          if (aggression < 10)
                            paste0("0"),
                          aggression)
    
    # Create the directory to save the plot and data (./plots/shower.name)
    save.dir <- paste0("./plots/", shower.name)
    dir.create(save.dir)
    
    #File to save most recently generated ggplot, what is currently displayed
    plot.file <- paste0(save.dir, "/", shower.name, ".png")
    ggsave(plot.file)
    
    # File to save the relavent information
    info.file <- paste0(save.dir, "/", shower.name, ".txt")
    info <- cbind(get.shower.info(), get.radiant.info())
    write.table(info,
                file = info.file,
                sep = "\t",
                row.names = FALSE)
  })
})
