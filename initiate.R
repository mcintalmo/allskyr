ensure.package <- function(package)
{
  package <- as.character(package)
  if (!require(package, character.only = TRUE))
  {
    install.packages(pkgs = package, repos = "http://cran.r-project.org")
    require(package, character.only = TRUE)
  }
}

ensure.package("astrolibR")
ensure.package("stringr")
ensure.package("gplots")
ensure.package("ggplot2")
ensure.package("shiny")
ensure.package('Directional')
ensure.package("dplyr")


source("event.R")
source("import.R")
source("radiant.R")
source("shower.R")
source("outlier.R")
source("fit.R")
