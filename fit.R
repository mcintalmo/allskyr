bivariate.PDF <- function(x1, x2) {
    #(Kenney and Keeping 1951, pp. 92 and 202-205;
    #   Whittaker and Robinson 1967, p. 329)
    mu1 <- mean(x1)
    sigma1 <- sd(x1)
    x1.gauss <- dnorm(x1, mu1, sigma1)
    
    mu2 <- mean(x2)
    sigma2 <- sd(x2)
    x2.gauss <- dnorm(x2, mu2, sigma2)
    
    covariance <- cov(x1, x2)
    
    rho <- covariance / (sigma1 * sigma2) # Correlation of x1 and x2
    
    z <- (x1 - mu1) ^ 2 / sigma1 ^ 2 + (x2 - mu2) ^ 2 / sigma2 ^ 2
           - 2 * rho * (x1 - mu1) * (x2 - mu2) / (sigma1 * sigma2)
    
    PDF <- exp(-z / (2 * (1 - rho ^ 2)))
             / (2 * pi * sigma1 * sigma2 * sqrt(1 - rho ^ 2))
    
    return(PDF)
  }


kent <- function(ra, dec) {
    #Convert to Euclidean, pass to kent.mle for fit, extract mean direction
    #  and kappa for "standard deviation"
    r <- equatorial.to.cartesian(ra, dec)
    kent <-
      kent.mle(r) # From Directional package. For data fit on a unit sphere
    print(c("kent runtime: ", kent$runtime))
  }
