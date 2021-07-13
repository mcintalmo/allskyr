numeric.date <- function(date, format = "%Y%m%d", tz = "UTC") {
  date <- strptime(date, format, tz = tz)
  date <- strftime(date, format, tz = tz)
  date <- as.numeric(date)
  return(date)
}

find.events <- function(start.date,
                        end.date = NULL,
                        events,
                        format = "%Y%m%d",
                        tz = "UTC") {
  # Sanity check
  if (is.null(end.date) && length(start.date) == 2) {
    end.date <- start.date[[2]]
    start.date <- start.date[[1]]
  }
  else if (length(start.date) != 1) {
    stop("Invalid length of start.date in shower.R ")
  }
  else if (length(end.date) != 1) {
    stop("Invalid length of end.date in shower.R ")
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
  ###### Old method, only checked sequential events
  #radiants <- c()
  #for(i in 2:length(events)){
  #   radiants <- rbind(radiants, t(radiant(events[[i]], events[[i - 1]])))
  #}
  #  radiants <- as.data.frame(radiants)
  #  names(radiants) <- c("ra", "dec")
  #
  ### Slightly newer method
  #  radiants <- sapply(events, function(event1){
  #    sapply(events, function(event2) {
  #     if(event.name(event1) != event.name(event2)){
  #       radiant(event1, event2)})})
  #     }
  #  n <- length(unlist(radiants))
  # radiants <- data.frame(ra = (unlist(radiants)[seq(1, n, 2)]),
  #                        dec = unlist(radiants)[seq(2, n, 2)])
  #}
  
  # Go over every event (except the last) and check the radiant with every
  #   event that comes after (including the last)
  radiants <- lapply(events[-n.events], function(event1) {
    next.event <- match(event.name(event1), event.name(events)) + 1
    if (verbose) {
      cat(next.event - 1, "/", n.events - 1, " events ")
    }
    lapply(events[next.event:n.events], function(event2) {
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
  })
  
  radiants <- bind_rows(radiants)
  
  return(radiants)
}
