library(shiny)

shinyUI(fluidPage(
  titlePanel("Meteor Shower Radiant"),
  sidebarPanel(
    selectInput(
      "name",
      "Shower Name:",
      c(
        "Quadrantids (010 QUA)",
        "γ-Ursae Minorids (404 GUM)",
        "α-Centaurids (102 ACE)",
        "γ-Normids (118 GNO)",
        "Lyrids (006 LYR)",
        "π-Puppids (137 PPU)",
        "η-Aquariids (031 ETA)",
        "η-Lyrids (145 ELY)",
        "Dayt. Arietids (171 ARI)",
        "June Bootids (170 JBO)",
        "Piscis Austr. (183 PAU)",
        "S. δ-Aquariids (005 SDA)",
        "α-Capricornids (001 CAP)",
        "Perseids (007 PER)",
        "κ-Cygnids (012 KCG)",
        "Aurigids (206 AUR)",
        "Sept. ε-Perseids (208 SPE)",
        "Dayt. Sextantids (221 DSX)",
        "Draconids (009 DRA)",
        "S. Taurids (002 STA)",
        "δ-Aurigids (224 DAU)",
        "ε-Geminids (023 EGE)",
        "Orionids (008 ORI)",
        "Leonis Minorids (022 LMI)",
        "N. Taurids (017 NTA)",
        "Leonids (013 LEO)",
        "α-Monocerotids (246 AMO)",
        "Nov. Orionids (250 NOO)",
        "Phoenicids (254 PHO)",
        "Puppid-Velids (301 PUP)",
        "Monocerotids (019 MON)",
        "σ-Hydrids (016 HYD)",
        "Geminids (004 GEM)",
        "Comae Ber. (020 COM)",
        "Dec. L. Minorids (032 DLM)",
        "Ursids (015 URS)"
      )
    ),
    selectInput(
      "year",
      "Shower Year:",
      c("2009", "2010", "2011", "2012", "2013", "2014", "2015",
        "2016")
    ),
    sliderInput(
      "aggression",
      "Removal Aggression Level",
      min = 0,
      max = 99,
      value = 0
    ),
    checkboxInput("remove.antiradiants", "Remove Antiradiants", value = TRUE),
    sliderInput(
      "nbins",
      "Number of Bins:",
      min = 1,
      max = 360,
      value = 45
    ),
    actionButton("save", "Save to File")
  ),
  
  mainPanel(
    plotOutput("radiant.plot"),
    tableOutput("shower.table"),
    tableOutput("radiant.table"),
    textOutput("event.info"),
    textOutput("intersect.info"),
    textOutput("outlier.info")
  )
))
