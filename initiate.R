ensure.package <- function(package)
{
  package <- as.character(package)
  if (!require(package, character.only=TRUE))
  {
    install.packages(pkgs = package, repos = "http://cran.r-project.org") 
    require(package, character.only=TRUE)
  }
}

ensure.package("astrolibR")
ensure.package("stringr")
ensure.package("gplots")
ensure.package("ggplot2")
ensure.package("shiny")

source("event.R")
source("import.R")
source("radiant.R")
source("shower.R")
source("outlier.R")