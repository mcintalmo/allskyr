update.events <- function(events.path = "./events", 
                          verbose = FALSE, 
                          save.file = "./save-files/events.sav"){
  
  event.files <- list.files(path = events.path, 
                            pattern = "ev_[0-9]{8}_[0-9]{6}[A-Z]_[0-9]{2}\\.txt", 
                            full.names = TRUE, recursive = TRUE)
  
  events <- lapply(event.files, function(event.file, verbose){
    if(verbose){cat("\tevent.file:", event.file, "...")}
      
    event.name <- sub(".*/", event.file, replacement = "")
    event.name <- sub("\\.txt", event.name, replacement="")
      
    meta <- read.delim(event.file, comment.char="", header = FALSE, skip = 1, 
                       nrows = 14, as.is = TRUE)
    meta <- sub("# *", "", meta[[1]])
    meta <- strsplit(meta, " : ")
    
    data <- read.table(event.file, as.is = TRUE, 
                       col.names = c("fr", "time", "sum", "seq", "cx", "cy", 
                                     "th", "phi", "lsp", "mag", "flag"))
    
    #if(data[(data$cx == data$cy),] == 0){
    #  return
    #}
      
    if(verbose){cat("X\n")}
      
    return(event(name = event.name,
                 file = event.file,
                 version = meta[[grep("version", meta)]][2],
                 num_fr = as.integer(meta[[grep("num_fr", meta)]][2]),
                 date =  strptime(meta[[grep("time", meta)]][2], "%Y%m%d %H:%M:%S", 
                                  tz =strsplit(meta[[grep("time", meta)]][2], " ")[[1]][3]),
                 unix = as.numeric(meta[[grep("unix", meta)]][2]),
                 ntp = meta[[grep("ntp", meta)]][2],
                 seq0 = as.integer(meta[[grep("seq", meta)]][2]),
                 mul = meta[[grep("mul", meta)]][2],
                 site = meta[[grep("site", meta)]][2],
                 latlon = as.numeric(strsplit(meta[[grep("latlon", meta)]][2], " ")[[1]]),
                 text = meta[[grep("text", meta)]][2],
                 plate = meta[[grep("plate", meta)]][2], 
                 geom = as.integer(strsplit(meta[[grep("geom", meta)]][2], " ")[[1]]),
                 reject = as.logical(as.numeric(meta[[grep("reject", meta)]][2])),
                 fr  = data$fr,
                 time = data$time,
                 sum  = data$sum, 
                 seq  = data$seq, 
                 cx   = data$cx, 
                 cy   = data$cy, 
                 th  = data$th, 
                 phi  = data$phi, 
                 lsp  = data$lsp,
                 mag  = data$mag, 
                 flag = as.logical(data$flag))
    )}, verbose)
  
  save(events, list = "events", file = save.file, verbose = verbose)
}

load.events <- function(load.file = "./save-files/events.sav", verbose = FALSE){
  load(load.file)
  return(events)
}

update.showers <- function(shower.file = c("./showers/shower_calendar_2010.txt", 
                                           "./showers/shower_calendar_2011.txt", 
                                           "./showers/shower_calendar_2012.txt", 
                                           "./showers/shower_calendar_2013.txt", 
                                           "./showers/shower_calendar_2014.txt",
                                           "./showers/shower_calendar_2015.txt",
                                           "./showers/shower_calendar_2016.txt"),
                                save.file = "./save-files/shower.sav",
                                year=NULL){
  
  shower.data <- lapply(shower.file, function(file, year){
    if(is.null(year)){
      year <- str_extract(file, "(?<=\\_)[0-9]{4}")
    }
    #raw.data <- readLines(file, encoding = "UTF-8")
    raw.data <- readLines(file)
    names <- str_match(raw.data, ".*(?= \\()")
    numbers <- str_match_all(raw.data, "[0-9]{3}(?= [A-Z])")
    abbrevs <- str_match_all(raw.data, "[A-Z]{3}(?=\\))")
    months <- str_match_all(raw.data, "[A-Z][a-z]{2}(?= )")
    values <- str_extract_all(raw.data, "\\+*\\-*[0-9]+\\.*[0-9]*")
    data <- sapply(1:length(raw.data), function(i, names, numbers, abbrevs, months, values, year){
      month.number = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
      
      if(nchar(values[[i]][[1]]) > 2){
        values[[i]] <- values[[i]][-1]
      }
      
      start.day <- values[[i]][[1]]
      start.month <- month.number[months[[i]][[1]] == month.abb]
      
      end.day <- values[[i]][[2]]
      end.month <- month.number[months[[i]][[2]] == month.abb]
      
      peak.day <- values[[i]][[3]]
      peak.month <- month.number[months[[i]][[3]] == month.abb]
      
      if((as.numeric(end.month) < as.numeric(start.month)) && (i < 3)){
        start.year <- as.character(as.numeric(year) - 1)
        end.year <- year
        if(as.numeric(peak.month) >= as.numeric(start.month)){
          peak.year <- start.year
        }
        else{
          peak.year <- end.year
        }
      }
      else if(as.numeric(end.month) < as.numeric(start.month)){
        start.year <- year
        end.year <- as.character(as.numeric(year) + 1)
        if(as.numeric(peak.month) < as.numeric(start.month)){
          peak.year <- end.year
        }
        else{
          peak.year <- start.year
        }
      }
      else {
        start.year <- year
        end.year <- year
        peak.year <- year
      }
      
      start.date <- paste(start.year, start.month, start.day, sep = "")
      end.date <- paste(end.year, end.month, end.day, sep = "")
      peak.date <- paste(peak.year, peak.month, peak.day, sep = "")
      
      gamma <- as.numeric(values[[i]][[4]])
      theo.ra <- as.numeric(values[[i]][[5]])
      theo.dec <- as.numeric(values[[i]][[6]])
      v <- as.numeric(values[[i]][[7]])
      r <- as.numeric(values[[i]][[8]])
  
      if(length(values[[i]]) < 9){
        zhr <- "Var"
      }
      else{
        zhr <- values[[i]][[9]]
      }
      
      return(c(names[[i]], numbers[[i]], abbrevs[[i]], start.date, end.date, peak.date, 
               gamma, theo.ra, theo.dec, v, r, zhr))
      
    }, names, numbers, abbrevs, months, values, year)
  
    data <- as.data.frame(t(data), stringsAsFactors = FALSE)
    
    names(data) <- c("name", "number", "abbrev", "start.date", "end.date", "peak.date", 
                     "gamma", "theo.ra", "theo.dec", "v", "r", "zhr")
    
    return(data)
  }, year)
  
  showers <- NULL
  for(data.frame in shower.data){
    showers <- rbind(showers, data.frame)
  }
  
  save(showers, list = "showers", file = save.file)
}

load.showers <- function(file = "./save-files/shower.sav"){
  load(file)
  return(showers)
}