library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Meteor Shower Radiant"),
  
  sidebarPanel(
    selectInput("name",
                "Shower Name:",
                c("Quadrantids", "α-Centaurids", "γ-Normids", "Lyrids", 
                  "π-Puppids", "η-Aquariids", "η-Lyrids", "June Bootids",
                  "Piscis Austrinids", "South. δ-Aquariids", "α-Capricornids", 
                  "Perseids",  "κ-Cygnids", "α-Aurigids", "Sept. Perseids", 
                  "δ-Aurigids", "Draconids", "ε-Geminids", "Orionids", 
                  "Leonis Minorids", "Southern Taurids", "Northern Taurids", 
                  "Leonids", "α-Monocerotids", "Phoenicids", "Puppid/Velids", 
                  "Monocerotids", "σ-Hydrids", "Geminids",  "Comae Berenicids", 
                  "Ursids", "Sept. ε-Perseids","Dec. Leonis Minorids", 
                  "Aurigids", "Piscis Austr.", "S. δ-Aquariids", "S. Taurids", 
                  "N. Taurids", "Puppid-Velids", "Comae Ber.", 
                  "Dec. L. Minorids", "Dayt. Arietids", "Dayt. Sextantids", 
                  "Nov. Orionids")),
    selectInput("year",
                "Shower Year:",
                c("2009", "2010", "2011", "2012", "2013", "2014", "2015", 
                  "2016")),
    checkboxInput("remove.outliers",
                  "Remove Outliers"),
    numericInput("iqr",
                 "IQR Value",
                 value = 1.5,
                 min = .1,
                 max = 20),
    sliderInput("nbins",
                "Number of Bins:",
                min = 1,
                max = 360,
                value = 25),
    actionButton("generate", "Generate Plot")
    
  ),
  
  mainPanel(
    plotOutput("radiant.plot"),
    
    tableOutput("shower.info"),
    
    textOutput("event.info"),
    
    textOutput("outlier.info")
  )
))