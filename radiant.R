#Returns the norm of the given vector
norm <- function(vector) {
  if (class(vector) == "list") {
    return(sapply(vector, function(vector) {
      return(sqrt(sum(vector ^ 2)))
    }))
  }
  if (class(vector) == "numeric" || class(vector) == "matrix") {
    return(sqrt(sum(vector ^ 2)))
  }
  stop("Invalid object passed to norm")
}

#Returns the normalized vector
normalize <- function(vector) {
  if (class(vector) == "list") {
    sapply(vector, function(vector) {
      return(vector / norm(vector))
    })
  }
  else if (class(vector) == "numeric") {
    return(vector / norm(vector))
  }
  else {
    stop("Invalid object passed to normalize")
  }
}

#Returns the cross product of two vectors
cross.product <- function(vector1, vector2) {
  if ((length(vector1) != 3) || (length(vector2) != 3)) {
    stop("Invalid dimensions for cross product (D[3] X D[3])")
  }
  
  i = vector1[[2]] * vector2[[3]] - vector1[[3]] * vector2[[2]]
  j = vector1[[3]] * vector2[[1]] - vector1[[1]] * vector2[[3]]
  k = vector1[[1]] * vector2[[2]] - vector1[[2]] * vector2[[1]]
  
  return(c(i, j, k))
}

#Converts from equatorial coordinates to cartesian coordinates
equatorial.to.cartesian <- function(ra, dec = NULL) {
  #If the data is being passed as a table, break it into two vectors
  if ((length(dec) == 0) && (length(ra) == 2)) {
    dec <- sapply(ra, function(ra) {
      return(ra[2])
    })
    ra  <- sapply(ra, function(ra) {
      return(ra[1])
    })
  }
  #If the ha is provided, break it off
  else if (!is.null(names(ra)) &&
           all(names(ra) == c("ra", "dec", "ha"))) {
    dec <- ra$dec
    ha <- ra$ha
    ra <- ra$ra
  }
  #Null case
  else if ((length(ra) == 0) && (length(dec)) == 0) {
    return()
  }
  else if (length(ra) != length(dec)) {
    stop("Invalid length of ra or dec. Haulting execution.")
  }
  
  r <- sapply(1:length(ra), function(i, ra, dec) {
    x = cos(dec[[i]] * pi / 180) * cos(ra[[i]] * pi / 180)
    y = cos(dec[[i]] * pi / 180) * sin(ra[[i]] * pi / 180)
    z = sin(dec[[i]] * pi / 180)
    return(c(x, y, z))
  }, ra, dec)
  
  return(r)
}

radiant <- function(event1, event2) {
  #Get cartesian coordinates for the start and end points of the events
  r1.start <-
    equatorial.to.cartesian(event.start.equatorial(event1))
  r1.end   <- equatorial.to.cartesian(event.end.equatorial(event1))
  
  r2.start <-
    equatorial.to.cartesian(event.start.equatorial(event2))
  r2.end   <- equatorial.to.cartesian(event.end.equatorial(event2))
  
  #Crossing the start point with the end point gives a norm for the plane
  #  in which the movement exists
  n1 <- cross.product(r1.start, r1.end)
  n2 <- cross.product(r2.start, r2.end)
  
  #Crossing the norm of the resulting plane above yields the vector of the
  #  line intersecting the two plance, which points towards (or directly away
  #  from) the point of intersection
  rad <- normalize(cross.product(n1, n2))
  
  
  #Check if the antiradiant was found. Test if the result is
  #  closer to the start of the events (thus its source) or the end of the
  #   events (thus its termination).
  if ((norm(r1.start - rad) > norm(r1.end - rad)) &&
      (norm(r2.start - rad) > norm(r2.end - rad))) {
    #confirmed antiradiant
    rad <- -rad
    antirad <- 1
  } else if ((norm(r1.start - rad) > norm(r1.end - rad)) ||
             (norm(r2.start - rad) > norm(r2.end - rad))) {
    #An error flag, this result doesn't correlate to anything that would
    #  be expected to happen if both events were from a meteor shower.
    antirad <- -1
  } else
    antirad <- 0 #confirmed radiant
  
  x <- rad[[1]]
  y <- rad[[2]]
  z <- rad[[3]]
  
  #Convert cartesian coordinates to equatorial
  rho <- norm(c(x, y))
  ra <- (acos(x / rho) * 180 / pi) %% 360
  
  if (y < 0) {
    ra <- 360 - ra
  }
  
  rho <- norm(rad)
  dec <- asin(z / rho) * 180 / pi
  
  return(data.frame(
    ra = ra,
    dec = dec,
    antirad = antirad
  ))
}
