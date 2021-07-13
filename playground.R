###############################################################################
# This is a file only for keeping my thoughts in order. Disregard if looking 
#  for any meaningful insight.
###############################################################################

source("initiate.R")

events <- load.events()
showers <- load.showers()

# For finding showers with a lot of events
# max = 0
# max_i = 0
# for (i in 1:nrow(showers)){
#   test.shower <- showers[i,]
#  shower.events <- find.events(test.shower$start.date,
#                               test.shower$end.date, events)
#   if (length(shower.events) > max){
#     max <- length(shower.events)
#     max_i <- i
#     print(paste(test.shower$name, " ", length(shower.events), " ", i))
#   }
# }


perseids <- showers[12,]
test.shower <- perseids

if(file.exists(paste("./save-files/radiants/", test.shower$abbrev, 
                     substr(test.shower$start.date, 1, 4), ".txt", sep=""))){
  test.radiants <- load.radiant(test.shower)
} else{
  shower.events <- find.events(test.shower$peak.date,
                               test.shower$peak.date,
                               events)
  test.radiants <- shower.radiant(shower.events)
}

suspected.events <- suspect.events(test.shower,
                                   remove.antiradiant=FALSE, 
                                   aggression=0.25)
test.radiants <- remove.event(test.radiants, suspected.events)

r <- equatorial.to.cartesian(test.radiants$ra, test.radiants$dec)
r <- t(r)
kent <- kent.mle(r)

mean <- kent$G[,1]
x <- mean[1]
y <- mean[2]
z <- mean[3]

#Convert cartesian coordinates to equatorial
rho <- norm(c(x, y))
ra.mean <- (acos(x / rho) * 180 / pi) %% 360

if(y < 0){
  ra.mean <- 360 - ra.mean
}

rho <- norm(mean)
dec.mean <- asin(z / rho) * 180 / pi
#Convert cartesian coordinates to equatorial
rho <- norm(c(x, y))
ra.mean <- (acos(x / rho) * 180 / pi) %% 360

if(y < 0){
  ra.mean <- 360 - ra.mean
}

print(paste(ra.mean, dec.mean))

kappa <- kent$param[1]

sphereplot(r)
