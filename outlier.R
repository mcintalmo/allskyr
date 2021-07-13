iqr.threshold <- function(x, multiplier = 1.5) {
  quartiles <- quantile(x)
  
  lower.quartile <- quartiles[[2]]
  upper.quartile <- quartiles[[4]]
  iqr <- IQR(x)
  
  lower.threshold <- lower.quartile - (iqr * multiplier)
  upper.threshold <- upper.quartile + (iqr * multiplier)
  return(data.frame(lower = lower.threshold, upper = upper.threshold))
}

mode2d <- function(hist) {
  mode <- which(hist$counts == max(hist$counts), arr.ind = TRUE)
  
  ra.mode <- hist$x[mode[[1]]]
  dec.mode <- hist$y[mode[[2]]]
  return(data.frame(ra.mode = ra.mode, dec.mode = dec.mode))
}

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


suspect.events <-
  function(shower,
           remove.antiradiant = FALSE,
           aggression = 0.25) {
    radiant <- load.radiant(shower) #Radiant to be checked
    n.radiants <- length(radiant[, 1])
    n.events <- length(union(radiant$event1, radiant$event2))
    if (remove.antiradiant) {
      index <- which(radiant$antirad != 0)
    }
    else{
      #Index of values that are suspect (antirad != 0)
      index <- which(radiant$antirad < 0) 
    }
    
    #as.character prevents factorization
    events <- c(as.character(radiant$event1[index]),
                as.character(radiant$event2[index])) 
    events <- table(events) #Table with the frequency of each event
    
    events <- events[events > n.events * (1 - aggression)]
    
    return(names(events))
  }

remove.event <- function(radiant, event) {
  radiant <- radiant[!radiant$event1 %in% event,]
  radiant <- radiant[!radiant$event2 %in% event,]
  return(radiant)
}
