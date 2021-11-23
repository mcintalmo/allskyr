# suspect.events take the radiants of a meteor shower and an aggression
#  parameter as input and returns all events that fail. The radiants contain an
#  antirad property and the two events that produced it. 
#  An antirad of 0 is true radiant, 1 is antiradiant, and -1 means
#  that intersection was neither the radiant nor antiradiant. The aggression
#  input determines how many -1's it takes for an event to be considered suspect.
#  e.g. an aggression of 0.25 flags events that have at least a %75 -1 rate.
#  An aggression of 0 has no effect, an aggression of 1 will remove all events.
suspect.events <- function(radiants, aggression = 0.5) {
  n.radiants <- nrow(radiants)
  # union is used to get unique values of the events (each radiant associated with 2 events)
  n.events <- length(union(radiants$event1, radiants$event2))
  
  #Index of weird radiants (antirad < 0)
  sus.index <- which(radiants$antirad < 0)
  
  # Combine all events with a -1 into a list. They will occur a number of times
  #  equal to the number of radiants they are assocaited with.
  sus.events <- c(radiants$event1[sus.index], radiants$event2[sus.index])
  sus.events <- table(sus.events) #Table with the frequency of each event
  
  # Keep only the events that fall within the threshold.
  sus.events <- sus.events[sus.events > n.events * (1 - aggression)]
  return(names(sus.events))
}

# remove.event removes all radiants from the given data frame that are
#  associated with the given event.
remove.event <- function(radiants, event) {
  radiants <- radiants[!radiants$event1 %in% event,]
  radiants <- radiants[!radiants$event2 %in% event,]
  return(radiants)
}

# Deprecated. Part of old Shiny app.
iqr.threshold <- function(x, multiplier = 1.5) {
  quartiles <- quantile(x)
  
  lower.quartile <- quartiles[[2]]
  upper.quartile <- quartiles[[4]]
  iqr <- IQR(x)
  
  lower.threshold <- lower.quartile - (iqr * multiplier)
  upper.threshold <- upper.quartile + (iqr * multiplier)
  return(data.frame(lower = lower.threshold, upper = upper.threshold))
}

# Deprecated. Part of old Shiny app.
mode2d <- function(hist) {
  mode <- which(hist$counts == max(hist$counts), arr.ind = TRUE)
  
  ra.mode <- hist$x[mode[[1]]]
  dec.mode <- hist$y[mode[[2]]]
  return(data.frame(ra.mode = ra.mode, dec.mode = dec.mode))
}

# Deprecated. Part of old Shiny app.
center.on.point <- function(x,
                            point,
                            center = 180,
                            wrap = TRUE) {
  if (wrap) {
    x <- (x + center - point) %% (2 * center)
  }
  else{
    x <- x + center - point
  }
  return(x)
}

# Deprecated. Part of old Shiny app.
outlier.trim <-
  function(events,
           radiants,
           mode = NULL,
           nbins = 25,
           ...) {
    if (is.null(mode)) {
      radiant.hist <- hist2d(radiants, nbins = nbins, show = FALSE, ...)
      mode <- mode2d(radiant.hist)
    }
    
    radiants$ra <-
      center.on.point(radiants$ra, mode$ra, center = 180,
                      wrap = TRUE)
    
    ra.threshold <- iqr.threshold(radiants$ra, ...)
    dec.threshold <- iqr.threshold(radiants$dec, ...)
    
    
    outliers <- which((radiants$ra < ra.threshold$lower) |
                        (radiants$ra > ra.threshold$upper) |
                        (radiants$dec < dec.threshold$lower) |
                        (radiants$dec > dec.threshold$upper)
    )
    
    shift <- c(outliers[-1], outliers[[1]])
    
    outliers <- outliers[outliers + 1 == shift]
    
    return(events[-outliers])
  }
