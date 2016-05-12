event <- setClass("event",
                  slots = c(name = "character", #Name of the event ev_YYYYMMDD_HHMMSSX0_1
                            file = "character", #File the event was read from
                            version = "character", #Version of ASGARD that created the event
                            num_fr = "integer", #number of frames of the event
                            date = "POSIXlt", #Date and time at which the event occured
                            unix = "numeric", #unix time at which the event occured
                            ntp = "character", #UNKOWN
                            seq0 = "integer", #seq value at time 0
                            mul = "character", #UNKOWN
                            site = "character", #Site at which the event was recorded (numiercal value)
                            latlon = "numeric", #c(latitude, longitude, elevation)
                            text = "character", #site of the event (string value)
                            plate = "character", #platefile used to correct for theta and phi
                            geom = "integer", #UNKNOWN
                            reject = "logical", #Whether or not the event should be rejected
                            fr  = "integer", #frame number
                            time = "numeric", #time offset
                            sum  = "integer", #UNKNOWN
                            seq  = "integer", #UNKOWN
                            cx   = "numeric", #x coordinate of the detected meteor pixel
                            cy   = "numeric", #y coordinate of the detected meteor pixel
                            th  = "numeric", #spherical conversion of the x,y coordinates using platefile
                            phi  = "numeric", #spherical conversion of the x, y coordates using platefile
                            lsp  = "numeric", #UNKNOWN
                            mag  = "numeric", #Magnitude of the meteor
                            flag = "logical"), #Flag that something isn't quite right
                  
                  prototype = list(name = character(),
                                   file = character(),
                                   version = character(),
                                   num_fr = integer(),
                                   date = strptime("00000101000000", 
                                                   "%Y%m%d%H%M%S"),
                                   unix = numeric(), 
                                   ntp = character(),
                                   seq0 = integer(), 
                                   mul = character(),
                                   site = character(),
                                   latlon = numeric(),
                                   text = character(),
                                   plate = character(),
                                   geom = integer(),
                                   reject = logical(),
                                   fr  = integer(),
                                   time = numeric(),
                                   sum  = integer(),
                                   seq  = integer(),
                                   cx   = numeric(),
                                   cy   = numeric(),
                                   th  = numeric(),
                                   phi  = numeric(),
                                   lsp  = numeric(),
                                   mag  = numeric(),
                                   flag = logical()
                                   )
                  )

setGeneric("event.name", def = function(events){
  standardGeneric("event.name")
})

setMethod("event.name", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              return(events@name)
              }))
            })

setMethod("event.name", signature = "event", 
          definition = function(events){
            return(events@name)
          }
)

setGeneric("event.file", def = function(events){
  standardGeneric("event.file")
})

setMethod("event.file", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              return(events@file)
            }))
          })

setMethod("event.file", signature = "event", 
          definition = function(events){
            return(events@file)
          }
)

setGeneric("event.version", def = function(events){
  standardGeneric("event.version")
})

setMethod("event.version", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              return(events@version)
            }))
          })

setMethod("event.version", signature = "event", 
          definition = function(events){
            return(events@version)
          }
)

setGeneric("event.num_fr", def = function(events){
  standardGeneric("event.num_fr")
})

setMethod("event.num_fr", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              return(events@num_fr)
            }))
          })

setMethod("event.num_fr", signature = "event", 
          definition = function(events){
            return(events@num_fr)
          }
)

setGeneric("event.date", def = function(events){
  standardGeneric("event.date")
})

setMethod("event.date", signature = "list", 
          definition = function(events){
            return(lapply(events, function(events){
              return(events@date)
            }))
          })

setMethod("event.date", signature = "event", 
          definition = function(events){
            return(events@date)
          }
)

setGeneric("event.unix", def = function(events){
  standardGeneric("event.unix")
})

setMethod("event.unix", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              return(events@unix)
            }))
          })

setMethod("event.unix", signature = "event", 
          definition = function(events){
            return(events@unix)
          }
)

setGeneric("event.ntp", def = function(events){
  standardGeneric("event.ntp")
})

setMethod("event.ntp", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              return(events@ntp)
            }))
          })

setMethod("event.ntp", signature = "event", 
          definition = function(events){
            return(events@ntp)
          }
)

setGeneric("event.seq0", def = function(events){
  standardGeneric("event.seq0")
})

setMethod("event.seq0", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              return(events@seq0)
            }))
          })

setMethod("event.seq0", signature = "event", 
          definition = function(events){
            return(events@seq0)
          }
)

setGeneric("event.mul", def = function(events){
  standardGeneric("event.mul")
})

setMethod("event.mul", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              return(events@mul)
            }))
          })

setMethod("event.mul", signature = "event", 
          definition = function(events){
            return(events@mul)
          }
)

setGeneric("event.site", def = function(events){
  standardGeneric("event.site")
})

setMethod("event.site", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              return(events@site)
            }))
          })

setMethod("event.site", signature = "event", 
          definition = function(events){
            return(events@site)
          }
)

setGeneric("event.latlon", def = function(events){
  standardGeneric("event.latlon")
})

setMethod("event.latlon", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              return(events@latlon)
            }))
          })

setMethod("event.latlon", signature = "event", 
          definition = function(events){
            return(events@latlon)
          }
)

setGeneric("event.text", def = function(events){
  standardGeneric("event.text")
})

setMethod("event.text", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              return(events@text)
            }))
          })

setMethod("event.text", signature = "event", 
          definition = function(events){
            return(events@text)
          }
)

setGeneric("event.plate", def = function(events){
  standardGeneric("event.plate")
})

setMethod("event.plate", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              return(events@plate)
            }))
          })

setMethod("event.plate", signature = "event", 
          definition = function(events){
            return(events@plate)
          }
)

setGeneric("event.geom", def = function(events){
  standardGeneric("event.geom")
})

setMethod("event.geom", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              return(events@geom)
            }))
          })

setMethod("event.geom", signature = "event", 
          definition = function(events){
            return(events@geom)
          }
)

setGeneric("event.reject", def = function(events){
  standardGeneric("event.reject")
})

setMethod("event.reject", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              return(events@reject)
            }))
          })

setMethod("event.reject", signature = "event", 
          definition = function(events){
            return(events@reject)
          }
)

setGeneric("event.fr", def = function(events){
  standardGeneric("event.fr")
})

setMethod("event.fr", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              return(events@fr)
            }))
          })

setMethod("event.fr", signature = "event", 
          definition = function(events){
            return(events@fr)
          }
)

setGeneric("event.time", def = function(events){
  standardGeneric("event.time")
})

setMethod("event.time", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              return(events@time)
            }))
          })

setMethod("event.time", signature = "event", 
          definition = function(events){
            return(events@time)
          }
)

setGeneric("event.sum", def = function(events){
  standardGeneric("event.sum")
})

setMethod("event.sum", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              return(events@sum)
            }))
          })

setMethod("event.sum", signature = "event", 
          definition = function(events){
            return(events@sum)
          }
)

setGeneric("event.seq", def = function(events){
  standardGeneric("event.seq")
})

setMethod("event.seq", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              return(events@seq)
            }))
          })

setMethod("event.seq", signature = "event", 
          definition = function(events){
            return(events@seq)
          }
)

setGeneric("event.cx", def = function(events){
  standardGeneric("event.cx")
})

setMethod("event.cx", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              return(events@cx)
            }))
          })

setMethod("event.cx", signature = "event", 
          definition = function(events){
            return(events@cx)
          }
)

setGeneric("event.cy", def = function(events){
  standardGeneric("event.cy")
})

setMethod("event.cy", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              return(events@cy)
            }))
          })

setMethod("event.cy", signature = "event", 
          definition = function(events){
            return(events@cy)
          }
)
   
setGeneric("event.th", def = function(events){
  standardGeneric("event.th")
})

setMethod("event.th", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              return(events@th)
            }))
          })

setMethod("event.th", signature = "event", 
          definition = function(events){
            return(events@th)
          }
)

setGeneric("event.phi", def = function(events){
  standardGeneric("event.phi")
})

setMethod("event.phi", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              return(events@phi)
            }))
          })

setMethod("event.phi", signature = "event", 
          definition = function(events){
            return(events@phi)
          }
)
setGeneric("event.lsp", def = function(events){
  standardGeneric("event.lsp")
})

setMethod("event.lsp", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              return(events@lsp)
            }))
          })

setMethod("event.lsp", signature = "event", 
          definition = function(events){
            return(events@lsp)
          }
)

setGeneric("event.mag", def = function(events){
  standardGeneric("event.mag")
})

setMethod("event.mag", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              return(events@mag)
            }))
          })

setMethod("event.mag", signature = "event", 
          definition = function(events){
            return(events@mag)
          }
)

setGeneric("event.flag", def = function(events){
  standardGeneric("event.flag")
})

setMethod("event.flag", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              return(events@flag)
            }))
          })

setMethod("event.flag", signature = "event", 
          definition = function(events){
            return(events@flag)
          }
)

setGeneric("event.alt", def = function(events){
  standardGeneric("event.alt")
})

setMethod("event.alt", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              return(abs(90 - events@th))
            }))
          })

setMethod("event.alt", signature = "event", 
          definition = function(events){
            return(abs(90 - events@th))
          }
)

setGeneric("event.az", def = function(events){
  standardGeneric("event.az")
})

setMethod("event.az", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              return((450 - events@phi) %% 360)
            }))
          })

setMethod("event.az", signature = "event", 
          definition = function(events){
            return((450 - events@phi) %% 360)
          }
)

setGeneric("event.julian.date", def = function(events){
  standardGeneric("event.julian.date")
})

setMethod("event.julian.date", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              julian.days <- julian(event.date(events), origin = as.POSIXct("01-01-01", tz = "UTC"))
              return(as.numeric(julian.days))
            }))
          })

setMethod("event.julian.date", signature = "event", 
          definition = function(events){
            julian.days <- julian(event.date(events), origin = as.POSIXct("01-01-01", tz = "UTC"))
            return(as.numeric(julian.days))
          }
)

setGeneric("event.ra", def = function(events){
  standardGeneric("event.ra")
})

setMethod("event.ra", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              capture.output(equatorial <- hor2eq(event.alt(events), event.az(events), event.julian.date(events), lat = event.latlon(events)[1], lon = event.latlon(events)[2], altitude = event.latlon(events)[3]), file = 'NUL')
              return(equatorial$ra)
            }))
          })

setMethod("event.ra", signature = "event", 
          definition = function(events){
            capture.output(equatorial <- hor2eq(event.alt(events), event.az(events), event.julian.date(events), lat = event.latlon(events)[1], lon = event.latlon(events)[2], altitude = event.latlon(events)[3]), file = 'NUL')
            return(equatorial$ra)
            }
)

setGeneric("event.dec", def = function(events){
  standardGeneric("event.dec")
})

setMethod("event.dec", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              capture.output(equatorial <- hor2eq(event.alt(events), event.az(events), event.julian.date(events), lat = event.latlon(events)[1], lon = event.latlon(events)[2], altitude = event.latlon(events)[3]), file = 'NUL')
              return(equatorial$dec)
            }))
          })

setMethod("event.dec", signature = "event", 
          definition = function(events){
            capture.output(equatorial <- hor2eq(event.alt(events), event.az(events), event.julian.date(events), lat = event.latlon(events)[1], lon = event.latlon(events)[2], altitude = event.latlon(events)[3]), file = 'NUL')
            return(equatorial$dec)
          }
)

setGeneric("event.ha", def = function(events){
  standardGeneric("event.ha")
})

setMethod("event.ha", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              capture.output(equatorial <- hor2eq(event.alt(events), event.az(events), event.julian.date(events), lat = event.latlon(events)[1], lon = event.latlon(events)[2], altitude = event.latlon(events)[3]), file = 'NUL')
              return(equatorial$ha)
            }))
          })

setMethod("event.ha", signature = "event", 
          definition = function(events){
            capture.output(equatorial <- hor2eq(event.alt(events), event.az(events), event.julian.date(events), lat = event.latlon(events)[1], lon = event.latlon(events)[2], altitude = event.latlon(events)[3]), file = 'NUL')
            return(equatorial$ha)
          }
)

setGeneric("event.equatorial", def = function(events){
  standardGeneric("event.equatorial")
})

setMethod("event.equatorial", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              capture.output(equatorial <- hor2eq(event.alt(events), event.az(events), event.julian.date(events), lat = event.latlon(events)[1], lon = event.latlon(events)[2], altitude = event.latlon(events)[3]), file = 'NUL')
              return(equatorial)
            }))
          })

setMethod("event.equatorial", signature = "event", 
          definition = function(events){
            capture.output(equatorial <- hor2eq(event.alt(events), event.az(events), event.julian.date(events), lat = event.latlon(events)[1], lon = event.latlon(events)[2], altitude = event.latlon(events)[3]), file = 'NUL')
            return(equatorial)
          }
)

setGeneric("event.start.equatorial", def = function(events){
  standardGeneric("event.start.equatorial")
})

setMethod("event.start.equatorial", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              latlon <- events@latlon
              alt <- abs(90 - events@th[[1]])
              az <- (90 - events@phi[[1]]) %% 360
              capture.output(equatorial <- hor2eq(alt, az, event.julian.date(events), lat = latlon[[1]], lon = latlon[[2]], altitude = latlon[[3]]), file = 'NUL')
              return(equatorial)
            }))
          })

setMethod("event.start.equatorial", signature = "event", 
          definition = function(events){
            latlon <- events@latlon
            alt <- abs(90 - events@th[[1]])
            az <- (90 - events@phi[[1]]) %% 360
            capture.output(equatorial <- hor2eq(alt, az, event.julian.date(events), lat = latlon[[1]], lon = latlon[[2]], altitude = latlon[[3]]), file = 'NUL')
            return(equatorial)
          }
)

setGeneric("event.end.equatorial", def = function(events){
  standardGeneric("event.end.equatorial")
})

setMethod("event.end.equatorial", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              latlon <- events@latlon
              end <- events@num_fr
              alt <- abs(90 - events@th[[end]])
              az <- (90 - events@phi[[end]]) %% 360
              capture.output(equatorial <- hor2eq(alt, az, event.julian.date(events), lat = latlon[[1]], lon = latlon[[2]], altitude = latlon[[3]]), file = 'NUL')
              return(equatorial)
            }))
          })

setMethod("event.end.equatorial", signature = "event", 
          definition = function(events){
            latlon <- events@latlon
            end <- events@num_fr
            alt <- abs(90 - events@th[[end]])
            az <- (90 - events@phi[[end]]) %% 360
            capture.output(equatorial <- hor2eq(alt, az, event.julian.date(events), lat = latlon[[1]], lon = latlon[[2]], altitude = latlon[[3]]), file = 'NUL')
            return(equatorial)
          }
)

setGeneric("event.year", def = function(events){
  standardGeneric("event.year")
})

setMethod("event.year", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              return(strftime(events@date, "%Y"))
            }))
          })

setMethod("event.year", signature = "event", 
          definition = function(events){
            return(strftime(events@date, "%Y"))
          }
)

setGeneric("event.month", def = function(events){
  standardGeneric("event.month")
})

setMethod("event.month", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              return(strftime(events@date, "%m"))
            }))
          })

setMethod("event.month", signature = "event", 
          definition = function(events){
            return(strftime(events@date, "%m"))
          }
)

setGeneric("event.day", def = function(events){
  standardGeneric("event.day")
})

setMethod("event.day", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              return(strftime(events@date, "%d"))
            }))
          })

setMethod("event.day", signature = "event", 
          definition = function(events){
            return(strftime(events@date, "%d"))
          }
)

setGeneric("event.hour", def = function(events){
  standardGeneric("event.hour")
})

setMethod("event.hour", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              return(strftime(events@date, "%H"))
            }))
          })

setMethod("event.hour", signature = "event", 
          definition = function(events){
            return(strftime(events@date, "%H"))
          }
)

setGeneric("event.minute", def = function(events){
  standardGeneric("event.minute")
})

setMethod("event.minute", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              return(strftime(events@date, "%M"))
            }))
          })

setMethod("event.minute", signature = "event", 
          definition = function(events){
            return(strftime(events@date, "%M"))
          }
)

setGeneric("event.second", def = function(events){
  standardGeneric("event.second")
})

setMethod("event.second", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              return(strftime(events@date, "%S"))
            }))
          })

setMethod("event.second", signature = "event", 
          definition = function(events){
            return(strftime(events@date, "%S"))
          }
)

setGeneric("event.calendar.date", def = function(events){
  standardGeneric("event.calendar.date")
})

setMethod("event.calendar.date", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              return(strftime(events@date, "%Y%m%d"))
            }))
          })

setMethod("event.calendar.date", signature = "event", 
          definition = function(events){
            return(strftime(events@date, "%Y%m%d"))
          }
)

setGeneric("event.clock.time", def = function(events){
  standardGeneric("event.clock.time")
})

setMethod("event.clock.time", signature = "list", 
          definition = function(events){
            return(sapply(events, function(events){
              return(strftime(events@date, "%H%M%S"))
            }))
          })

setMethod("event.clock.time", signature = "event", 
          definition = function(events){
            return(strftime(events@date, "%H%M%S"))
          }
)

setGeneric("event.get", def = function(events, name){
  standardGeneric("event.get")
})

setMethod("event.get", signature = c("list", "character"), 
          definition = function(events, name){
            return(sapply(events, function(events){
              return(slot(events, name))
            }))
          })

setMethod("event.get", signature = c("event", "character"), 
          definition = function(events, name){
            return(slot(events, name))
          }
)