# Convert, Import, and Export Event Files ------------------------------------
read.event <- function(event.file, verbose = TRUE) {
  if (verbose) {
    cat("\tReading event:", event.file, "... ")
  }
  
  event.name <- sub(".*/", event.file, replacement = "")
  event.name <- sub("\\.txt", event.name, replacement = "")
  
  meta <-
    read.delim(
      event.file,
      comment.char = "",
      header = FALSE,
      skip = 1,
      nrows = 14,
      as.is = TRUE
    )
  meta <- sub("# *", "", meta[[1]])
  meta <- strsplit(meta, " : ")
  
  data <- read.table(
    event.file,
    as.is = TRUE,
    col.names = c(
      "fr",
      "time",
      "sum",
      "seq",
      "cx",
      "cy",
      "th",
      "phi",
      "lsp",
      "mag",
      "flag"
    )
  )
  
  if (any(data$cx == 0) ||
      any(data$cy == 0)) {
    ### IF THIS DATA IS IN THE MASK, SOMETHING IS WRONG
    if (verbose)
      cat("Rejected\n")
    return()
  }
  
  if (verbose) {
    cat("Read\n")
  }
  
  return(
    event(
      name = event.name,
      file = event.file,
      version = meta[[grep("version", meta)]][2],
      num_fr = as.integer(meta[[grep("num_fr", meta)]][2]),
      date = strptime(meta[[grep("time", meta)]][2],
                      "%Y%m%d %H:%M:%S",
                      tz = strsplit(meta[[grep("time", meta)]][2], " ")[[1]][3]),
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
      fr   = data$fr,
      time = data$time,
      sum  = data$sum,
      seq  = data$seq,
      cx   = data$cx,
      cy   = data$cy,
      th   = data$th,
      phi  = data$phi,
      lsp  = data$lsp,
      mag  = data$mag,
      flag = as.logical(data$flag)
    )
  )
}

get.events <- function(events.path = "./events",
                       verbose = TRUE,
                       save.file = "./save-files/events.sav") {
  if (verbose)
    cat("Reading events from", events.path, "\n")
  event.files <- list.files(
    path = events.path,
    pattern = "ev_[0-9]{8}_[0-9]{6}[A-Z]_[0-9]{2}\\.txt",
    full.names = TRUE,
    recursive = TRUE
  )
  
  events <- lapply(event.files, read.event, verbose)
  
  rejected <- sapply(events, is.null)
  
  events <- events[!rejected]
  
  if (verbose) {
    cat("Events found in",
        events.path,
        "read.The following events were rejected:\n\t")
    cat(event.file(events[rejected]), sep = "\n\t")
  }
  
  return(events)
}

save.events <-
  function(events,
           save.file = "./save-files/events.sav",
           verbose = TRUE) {
    return(save(
      events,
      list = "events",
      file = save.file,
      verbose = verbose
    ))
  }

load.events <-
  function(load.file = "./save-files/events.sav",
           verbose = TRUE) {
    load(load.file)
    return(events)
  }

update.events <-
  function(save.file = "./save-files/events.sav/",
           verbose = TRUE) {
    events <- get.events()
    return(save.events(events, save.file = save.file, verbose = verbose))
  }

# Convert, Import, and Export Shower Files -----------------------------------
read.shower <- function(file, year) {
  if (is.null(year)) {
    year <- str_extract(file, "(?<=\\_)[0-9]{4}")
  }
  raw.data <- readLines(file, encoding = "UTF-8")
  names <- str_match(raw.data, ".*(?= \\()")
  numbers <- str_match_all(raw.data, "[0-9]{3}(?= [A-Z])")
  abbrevs <- str_match_all(raw.data, "[A-Z]{3}(?=\\))")
  months <- str_match_all(raw.data, "[A-Z][a-z]{2}(?= )")
  values <- str_extract_all(raw.data, "\\+*\\-*[0-9]+\\.*[0-9]*")
  data <-
    sapply(1:length(raw.data), function(i,
                                        names,
                                        numbers,
                                        abbrevs,
                                        months,
                                        values,
                                        year) {
      month.number = c("01",
                       "02",
                       "03",
                       "04",
                       "05",
                       "06",
                       "07",
                       "08",
                       "09",
                       "10",
                       "11",
                       "12")
      
      if (nchar(values[[i]][[1]]) > 2) {
        values[[i]] <- values[[i]][-1]
      }
      
      start.day <- values[[i]][[1]]
      start.month <- month.number[months[[i]][[1]] == month.abb]
      
      end.day <- values[[i]][[2]]
      end.month <- month.number[months[[i]][[2]] == month.abb]
      
      peak.day <- values[[i]][[3]]
      peak.month <- month.number[months[[i]][[3]] == month.abb]
      
      if ((as.numeric(end.month) < as.numeric(start.month)) &&
          (i < 3)) {
        start.year <- as.character(as.numeric(year) - 1)
        end.year <- year
        if (as.numeric(peak.month) >= as.numeric(start.month)) {
          peak.year <- start.year
        }
        else{
          peak.year <- end.year
        }
      }
      else if (as.numeric(end.month) < as.numeric(start.month)) {
        start.year <- year
        end.year <- as.character(as.numeric(year) + 1)
        if (as.numeric(peak.month) < as.numeric(start.month)) {
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
      
      start.date <-
        paste(start.year, start.month, start.day, sep = "")
      end.date <- paste(end.year, end.month, end.day, sep = "")
      peak.date <- paste(peak.year, peak.month, peak.day, sep = "")
      
      gamma <- as.numeric(values[[i]][[4]])
      theo.ra <- as.numeric(values[[i]][[5]])
      theo.dec <- as.numeric(values[[i]][[6]])
      v <- as.numeric(values[[i]][[7]])
      r <- as.numeric(values[[i]][[8]])
      
      if (length(values[[i]]) < 9) {
        zhr <- "Var"
      }
      else{
        zhr <- values[[i]][[9]]
      }
      
      return(
        c(
          names[[i]],
          numbers[[i]],
          abbrevs[[i]],
          start.date,
          end.date,
          peak.date,
          gamma,
          theo.ra,
          theo.dec,
          v,
          r,
          zhr
        )
      )
      
    }, names, numbers, abbrevs, months, values, year)
  
  #StringsAsFactors = FALSE slows it down and uses more memory,
  #  but was causing problems in comparisons down the line
  data <-
    as.data.frame(t(data), stringsAsFactors = FALSE)
  
  names(data) <-
    c(
      "name",
      "number",
      "abbrev",
      "start.date",
      "end.date",
      "peak.date",
      "gamma",
      "theo.ra",
      "theo.dec",
      "v",
      "r",
      "zhr"
    )
  
  return(data)
}

get.showers <-
  function(years = c("2010", "2011", "2012", "2013", "2014", "2015", "2016"),
           verbose = TRUE) {
    shower <- NULL
    for (year in years) {
      shower.file <-
        paste("./showers/shower_calendar_", year, ".txt", sep = "")
      
      if (verbose)
        cat("Getting showers from", shower.file, ". . .")
      
      shower.data <- read.shower(shower.file, year)
      shower <- rbind(shower, shower.data)
      
      if (verbose)
        cat("X\n")
    }
    
    return(shower)
  }

save.showers <-
  function(showers,
           file = "./save-files/shower.sav",
           verbose = TRUE) {
    if (verbose)
      cat("Saving showers to", file, "...")
    save(showers, list = "showers", file = file)
    if (verbose)
      cat("X\n")
  }

load.showers <-
  function(file = "./save-files/shower.sav", verbose = TRUE) {
    if (verbose)
      cat("Loading showers from", file, ". . . ")
    load(file)
    if (verbose)
      cat("X\n")
    return(showers)
  }

update.showers <-
  function(file = "./save-files/shower.sav", verbose = TRUE) {
    if (verbose)
      cat("Saving showers to in ", file, " . . . ")
    showers <- get.showers()
    save.showers(showers, file = file, verbose = verbose)
    if (verbose)
      cat(file, "X\n")
  }

# Import and Create and Export Radiant Files ----------------------------------
get.radiant <-
  function(shower,
           radiant.path = "./save-files/radiants/",
           verbose = TRUE) {
    shower.name <-
      paste(shower$abbrev, substr(shower$start.date, 1, 4), sep = "")
    if (verbose)
      cat("Getting radiants for ", shower.name, " . . . ")
    shower.events <-
      find.events(shower$start.date, shower$end.date, events)
    if (length(shower.events) < 2) {
      if (verbose)
        message("Not enough events provided to produce radiant.\n")
      return()
    }
    else{
      radiant <- shower.radiant(shower.events)
      if (verbose)
        cat("X\n")
      return(radiant)
    }
  }

save.radiant <- function(shower,
                         radiant = get.radiant(shower),
                         radiant.path = "./save-files/radiants/",
                         verbose = TRUE) {
  if (is.null(radiant)) {
    message("Null radiant. Will not save.")
    return()
  }
  shower.name <-
    paste(shower$abbrev, substr(shower$start.date, 1, 4), sep = "")
  radiant.file <- paste(radiant.path, shower.name, ".txt", sep = "")
  if (verbose)
    cat("Saving ", shower.name, " radiant to ", radiant.file, " . . . ")
  write.table(radiant, file = radiant.file)
  if (verbose)
    cat("X\n")
}

load.radiant <-
  function(shower,
           radiant.path = "./save-files/radiants/",
           verbose = TRUE) {
    radiant.file <- paste(radiant.path,
                          shower$abbrev,
                          substr(shower$start.date, 1, 4),
                          ".txt",
                          sep = "")
    if (verbose)
      cat("Loading radiants from", radiant.file, " . . . ")
    if (file.info(radiant.file)$size < 10) {
      message(radiant.file, " failed to load. Too few intersections.")
      return()
    }
    radiant <- read.table(radiant.file)
    if (verbose)
      cat("X\n")
    return(radiant)
  }

update.radiant.files <-
  function(overwrite = FALSE,
           refresh.events = FALSE,
           refresh.showers = FALSE,
           radiant.path = "./save-files/radiants/",
           verbose = TRUE) {
    if (refresh.events)
      events <- get.events()
    else
      events <- load.events()
    if (refresh.showers)
      showers <- get.showers()
    else
      showers <- load.showers()
    showers <- split(showers, seq(nrow(showers)))
    
    radiants <- lapply(showers, function(shower) {
      shower.name <-
        paste(shower$abbrev, substr(shower$start.date, 1, 4), sep = "")
      radiant.file <- paste(radiant.path, shower.name, ".txt", sep = "")
      
      if (!overwrite && file.exists(radiant.file)) {
        if (verbose)
          cat(radiant.file, "already exists.\n")
        return()
      }
      
      if (overwrite &&
          file.exists(shower.file))
        cat("Overwriting ", radiant.file, "\n")
      
      save.radiant(shower)
    })
  }
