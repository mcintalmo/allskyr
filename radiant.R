#Returns the norm of the given vector 
norm <- function(vector){
  if(class(vector) == "list"){
    return(sapply(vector, function(vector){return(sqrt(sum(vector ^ 2)))}))
  }
  else if (class(vector) == "numeric"){
    return(sqrt(sum(vector ^ 2)))
  }
  else {
    stop("Invalid object passed to norm")
  }
}

#Returns the normalized vector
normalize <- function(vector){
  if(class(vector) == "list"){
    sapply(vector, function(vector){return(vector / norm(vector))})
  }
  else if(class(vector) == "numeric"){
    return(vector / norm(vector))
  }
  else {
    stop("Invalid object passed to normalize")
  }
}

#Returns the cross product of two vectors
cross.product <- function(vector1, vector2){
  if((length(vector1) != 3) || (length(vector2) != 3)){
    stop("Invalid dimensions for cross product (D[3] X D[3])")
  }
  
  i = vector1[[2]] * vector2[[3]] - vector1[[3]] * vector2[[2]]
  j = vector1[[3]] * vector2[[1]] - vector1[[1]] * vector2[[3]]
  k = vector1[[1]] * vector2[[2]] - vector1[[2]] * vector2[[1]]
  
  return(c(i, j, k))
}

#Converts from equatorial coordinates to cartesian coordinates
equatorial.to.cartesian <- function(ra, dec=NULL){
  #If the data is being passed as a table, break it into two vectors
  if((length(dec) == 0) && (length(ra) == 2)){
    dec <- sapply(ra, function(ra){return(ra[2])})
    ra  <- sapply(ra, function(ra){return(ra[1])})
  } 
  #If the ha is provided, break it off
  else if(all(names(ra) == c("ra", "dec", "ha"))) {
    dec <- ra$dec
    ha <- ra$ha
    ra <- ra$ra
  }
  #Null case
  else if((length(ra) == 0) && (length(dec)) == 0){
    return(c(0, 0, 0))
  }
  else if(length(ra) != length(dec)){
    stop("Invalid length of ra or dec. Haulting execution.")
  }
  
  r <- lapply(1:length(ra), function(i, ra, dec){
    x = cos(dec[[i]] * pi / 180) * cos(ra[[i]] * pi / 180)
    y = cos(dec[[i]] * pi / 180) * sin(ra[[i]] * pi / 180)
    z = sin(dec[[i]] * pi / 180)
    return(c(x, y, z))
  }, ra, dec)
  
  return(r)
}

#event.start.end <- function(events){
#  event.positions <- sapply(events, function(event){
#    r.start <- equatorial.to.cartesian(event.start.equatorial(event))
#    r.end <- equatorial.to.cartesian(event.start.equatorial(event))
#    
#    r.start.end <- list(c(r.start, r.end))
#    names(r.start.end) <- event.name(event)
#    return(r.start.end)
#  })
#  return(event.positions)
#}


#currently only calculates the radiant of two given events.
#need an optimized way to calculating radiant for more than two events
radiant <- function(event1, event2){
  
  r1 <- equatorial.to.cartesian(event.equatorial(event1))
  r1.start <- r1[[1]]
  r1.end <- r1[[event.num_fr(event1)]]
  
  r2 <- equatorial.to.cartesian(event.equatorial(event2))
  r2.start <- r2[[1]]
  r2.end <- r2[[event.num_fr(event2)]]
  
  n1 <- cross.product(r1.start, r1.end)
  n2 <- cross.product(r2.start, r2.end)
  
  rad <- normalize(cross.product(n1, n2))
  
  rpos <- norm(r1.start - rad) + norm(r2.start - rad)
  rneg <- norm(r1.start + rad) + norm(r2.start + rad)
  
  if(is.na(rneg) | is.na(rpos) | is.null(rneg) | is.null(rpos)){
    stop(event.name(event1), "\n", event.name(event2))
  }
  
  if(rneg < rpos){
    rad <- -rad
  }
  
  x <- rad[[1]]
  y <- rad[[2]]
  z <- rad[[3]]
  
  rho <- norm(c(x, y))
  rad.ra <- (acos(x / rho) * 180 / pi) %% 360
  
  if(y < 0){
    rad.ra <- 360 - rad.ra
  }
  
  rho <- norm(rad)
  rad.dec <- asin(z / rho) * 180 / pi
  
  return(c(rad.ra, rad.dec))
}

#Input list of events
#Take first two
#Find radiant 
#shift second event into the first event
#Get next event

#output list of radiants