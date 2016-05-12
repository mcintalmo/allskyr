numeric.date <- function(date, format, tz = "UTC"){
  date <- strptime(date, format, tz = tz)
  date <- strftime(date, "%Y%m%d", tz = "UTC")
  date <- as.numeric(date)
  return(date)
}

find.events <- function(start.date, end.date = NULL, events, 
                        format = "%Y%m%d", tz = "UTC"){
  if(is.null(end.date) && length(start.date) == 2){
    end.date <- start.date[[2]]
    start.date <- start.date[[1]]
  }
  else if(length(start.date) != 1){
    stop("Invalid length of start.date in shower.R ")
  }
  else if(length(end.date) != 1){
    stop("Invalid length of end.date in shower.R ")
  }
  
  start.date <- numeric.date(start.date, format, tz = tz)
  end.date <- numeric.date(end.date, format, tz = tz)
  event.dates <- as.numeric(event.calendar.date(events))
  
  return(events[(event.dates > start.date) & (event.dates < end.date)])
}

find.shower <- function(name, year, showers){
  return(showers[(((name == showers$name) | (name == showers$abbrev)) 
                  & year == substr(showers$start.date, 1, 4)),])
}

shower.radiant <- function(events){
  shower.radiant <- c()
  for(i in 2:length(events)){
    shower.radiant <- rbind(shower.radiant, t(radiant(events[[i]], events[[i - 1]])))
  }
  shower.radiant <- as.data.frame(shower.radiant)
  names(shower.radiant) <- c("ra", "dec")
  return(shower.radiant)
}