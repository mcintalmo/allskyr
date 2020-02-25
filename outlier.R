iqr.threshold <- function(x, multiplier = 1.5){
  quartiles <- quantile(x)
  
  lower.quartile <- quartiles[[2]]
  upper.quartile <- quartiles[[4]]
  iqr <- IQR(x)
  
  lower.threshold <- lower.quartile - (iqr * multiplier)
  upper.threshold <- upper.quartile + (iqr * multiplier)
  return(data.frame(lower = lower.threshold, upper = upper.threshold))
}

mode2d <- function(hist){
  mode <- which(hist$counts == max(hist$counts), arr.ind = TRUE)
  
  ra.mode <- hist$x[mode[[1]]]
  dec.mode <- hist$y[mode[[2]]]
  return(data.frame(ra = ra.mode, dec = dec.mode))
}

center.on.point <- function(x, point, center = 180, wrap = TRUE){
  if(wrap){
    x <- (x + center - point) %% (2 * center)
  }
  else{
    x <- x + center - point
  }
  return(x)
}

outlier.trim <- function(events, radiants, mode = NULL, nbins = 25, ...){
  if(is.null(mode)){
    radiant.hist <- hist2d(radiants, nbins = nbins, show = FALSE, ...)
    mode <- mode2d(radiant.hist)
  }
  
  radiants$ra <- center.on.point(radiants$ra, mode$ra, center = 180, 
                                 wrap = TRUE)
  
  ra.threshold <- iqr.threshold(radiants$ra, ...)
  dec.threshold <- iqr.threshold(radiants$dec, ...)
  
  
  outliers <- which((radiants$ra < ra.threshold$lower) | 
                    (radiants$ra > ra.threshold$upper) | 
                    (radiants$dec < dec.threshold$lower) | 
                    (radiants$dec > dec.threshold$upper))
  
  shift <- c(outliers[-1], outliers[[1]])
  
  outliers <- outliers[outliers + 1 == shift]
  
  return(events[-outliers])
}

bad.event.trim <- function(events, radiants, mode = NULL, nbins = 25, ...){
  if(is.null(mode)){
    radiant.hist <- hist2d(radiants, nbins = nbins, show = FALSE, ...)
    mode <- mode2d(radiant.hist)
  }
  
  radiants$ra <- center.on.point(radiants$ra, mode$ra, center = 180, 
                                 wrap = TRUE)
  
  ra.threshold <- iqr.threshold(radiants$ra, ...)
  dec.threshold <- iqr.threshold(radiants$dec, ...)
}