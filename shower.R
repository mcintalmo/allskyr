numeric.date <- function(date, format = "%Y%m%d", tz = "UTC") {
  date <- strptime(date, format, tz = tz)
  date <- strftime(date, format, tz = tz)
  date <- as.numeric(date)
  return(date)
}

find.events <- function(events,
                        start.date,
                        end.date = NULL,
                        format = "%Y%m%d",
                        tz = "UTC") {
  # Sanity check
  if (length(start.date) != 1) {
    stop("Invalid length of start.date in shower.R ")
  }
  else if (!is.null(end.date) && length(end.date) != 1) {
    stop("Invalid length of end.date in shower.R ")
  }
  else if (length(start.date == 1) && is.null(end.date)){
    end.date = start.date
  }
  
  start.date <- numeric.date(start.date, format = format, tz = tz)
  end.date <- numeric.date(end.date, format = format, tz = tz)
  event.dates <- as.numeric(event.calendar.date(events))
  
  return(events[(event.dates >= start.date) &
                  (event.dates <= end.date)])
}

find.shower <- function(name, year, showers) {
  return(showers[(((name == showers$name) |
                     (name == showers$abbrev) |
                     name == showers$number
  )
  & year == substr(showers$start.date, 1, 4)), ])
}

shower.radiant <- function(events, verbose = TRUE) {
  n.events <- length(events)
  if (n.events < 2) {
    # Cannot even be considered a shower with under 2 events
    print("Too few events passed to shower.radiant")
    return(data.frame(
      ra = NULL,
      dec = NULL,
      antirad = NULL,
      event1 = NULL,
      event2 = NULL
    ))
  }
  
  # Go over every event (except the last) and check the radiant with every
  #   event that comes after (including the last)
  radiants <- lapply(events[-n.events], function(event1) {
    next.event <- match(event.name(event1), event.name(events)) + 1
    if (verbose) {
      cat(next.event - 1, "/", n.events - 1, " events ")
    }
    radiant <- lapply(events[next.event:n.events], function(event2) {
      rad <- radiant(event1, event2)
      rad$event1 <- event.name(event1)
      rad$event2 <- event.name(event2)
      if (verbose) {
        cat(".")
      }
      return(rad)
    })
    if (verbose) {
      cat("X\n")
    }
    return(radiant)
  })
  
  radiants <- bind_rows(radiants)
  
  return(radiants)
}


mean.radiant <- function(events, remove.outliers = FALSE, verbose = TRUE){
  n.events <- length(events)
  if (n.events < 2){
    return(data.frame(n.events = n.events, n.radiants = 0))
  }
  # if(!recalc.radiants && file.exists(paste("./save-files/radiants/",
  #                                         shower$abbrev, 
  #                                         substr(shower$start.date, 1, 4),
  #                                         ".txt",
  #                                         sep=""))){
  #   radiants <- load.radiant(shower)
  #   n.events <- 0.5 * (sqrt(8 * nrow(radiants) + 1) - 1)
  # } else{
  radiants <- shower.radiant(events)
  # }
  n.radiants <- nrow(radiants)
  if (n.radiants < 2){
    return(data.frame(n.events = n.events, n.radiants = n.radiants))
  }
  
  r <- equatorial.to.cartesian(radiants$ra, radiants$dec)
  r <- t(r)
  
  if(remove.outliers){
    bad.events <- suspect.events(radiants, 0.5)
    n.bad.events <- length(bad.events)
    radiants <- remove.event(radiants, bad.events)
    n.bad.radiants <- n.radiants - nrow(radiants)
    n.radiants <- nrow(radiants)
    if (n.radiants < 2){
      return(data.frame(n.events = n.events,
                        n.radiants = n.radiants,
                        n.bad.events = n.bad.events,
                        n.bad.radiants = n.bad.radiants))
    }
  } else{
    n.bad.events <- -1
    n.bad.radiants <- -1
  }

  r <- equatorial.to.cartesian(radiants$ra, radiants$dec)
  r <- t(r)
  
  fishkent.p.value <- fishkent(r, B = 1)[2]
  kent <- kent.mle(r)
  vmf <- vmf.mle(r)
  # plot(radiants$ra, radiants$dec)
  
  x <- vmf$mu[1]
  y <- vmf$mu[2]
  z <- vmf$mu[3]
  
  
  #Convert cartesian coordinates to equatorial
  rho <- norm(c(x, y))
  mean.ra <- (acos(x / rho) * 180 / pi) %% 360
  
  if(y < 0){
    mean.ra <- 360 - mean.ra
  }
  
  rho <- norm(mean)
  mean.dec <- asin(z / rho) * 180 / pi
  #Convert cartesian coordinates to equatorial
  rho <- norm(c(x, y))
  mean.ra <- (acos(x / rho) * 180 / pi) %% 360
  
  if(y < 0){
    mean.ra <- 360 - mean.ra
  }
  
  kent.mu <- kent$G[,1]
  sdom <- sqrt(vmf$kappa ^ -1) / sqrt(n.events - n.bad.events) * 180 / pi
  
  return(data.frame(mean.ra = mean.ra,
                    mean.dec = mean.dec,
                    sdom = sdom,
                    n.events = n.events,
                    n.radiants = n.radiants,
                    n.bad.events = n.bad.events,
                    n.bad.radiants = n.bad.radiants,
                    fishkent.p.value = fishkent.p.value,
                    vmf.mu.x = vmf$mu[1],
                    vmf.mu.y = vmf$mu[2],
                    vmf.mu.z = vmf$mu[3],
                    vmf.kappa = vmf$kappa,
                    kent.mu.x = kent.mu[1],
                    kent.mu.y = kent.mu[2],
                    kent.mu.z = kent.mu[3],
                    kent.kappa = kent$param[1]))
}