###############################################################################
# This is a file (function) that is essentially a wrapper for more meaningful 
# functions in radiant.R. However, the specificity of the needs of this function
# made it appropriate to move it to a different space.
###############################################################################

save.radiant.statistics <- function(file = "radiant_statistics",
                                    all.showers = TRUE, 
                                    aggression = 0.0)
{
  # iniate.R just re-sources all files in the project.
  source("initiate.R")
  
  events <- load.events()
  showers <- load.showers()
  
  shower.data <- data.frame()
  if (all.showers){
    indices <- 1:nrow(showers) # all showers
  } else {
    indices <- c(12, 19, 29, 107, 139, 150, 192) # important shower indices
  }
  indices <- 12
  # Go over every given shower, find the mean radiant with associated statistics,
  #  and bind them all into a single data frame.
  for (i in indices){
    print(paste(i, showers[i,]$name)) 
    shower <- showers[i,]
    radiant.stats <- mean.radiant(events,
                                  shower,
                                  aggression = aggression)
    shower <- cbind(shower, radiant.stats)
    shower.data <- bind_rows(shower.data, shower)
  }
  
  # Go over the showers mentioned in Novacheck 2012 that data are available for.
  #  Find the mean radiant with associated statistics, and bind them all into 
  #  a same data frame as above.
  # novacheck.dates <- list('20091020', '20091120', '20091214', '20100103')
  # for (i in 1:length(novacheck.dates)){
  #   date <- novacheck.dates[[i]]
  #   shower.events <- find.events(events, date)
  #   radiant.stats <- mean.radiant(shower.events)
  #   radiant.stats$peak.date <- date
  #   shower.data <- bind_rows(shower.data, radiant.stats)
  # }
  # 
  # remove unnecessary data columns
  # shower.data <- subset(shower.data,
  #                       select = -c( ,start.date, end.date, gamma, v, r, zhr))
  # 
 
  
  if(all.showers){
    file = paste(file, "_all_showers", sep = "")
  }
  if(aggression > 0.0){
    file = paste(file, "_aggression_", str(ceiling(aggression * 100)), sep = "")
  }
  # write.csv(shower.data, file = paste(file, ".csv", sep = "")
  
  shower.data$name <- str_replace_all(shower.data$name, "α", "$\alpha$")
  
  print(xtable(shower.data),
        type = "latex",
        file = paste(file, ".tex", sep = ""))
}
