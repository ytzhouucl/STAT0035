library(sn)
library(knitr)
source("routines.R")

data(frontier)
print(frontier)

######################################################################################
# Maximum likelihood estimation
######################################################################################

# Optimisation step
OPTSN <- nlminb(start = c(0,0,3), objective = loglikSN)


# Maximum likelihood estimate
MLESN <- c(OPTSN$par[1],exp(OPTSN$par[2]),OPTSN$par[3])

kable(MLESN)


# Fitted SN
fitSN <- Vectorize(function(t) dsn(t, xi = MLESN[1], omega  = MLESN[2], alpha = MLESN[3], log = FALSE))

hist(frontier, probability = TRUE, breaks = 10, xlab = "x", ylab = "Density",
     cex.axis = 1.5, cex.lab = 1.5, pch =19) 
curve(fitSN, -1,4, add=TRUE, lwd = 2, col = "blue")
box()



##################################################################################################
# Profile likelihood of the parameters
##################################################################################################


# Required quantities
# Number of parameters
p <- 3
# (minus) Maximum value of the log-likelihood
ML <- OPTSN$objective


# Profile likelihoods

# Profile likelihood of Parameter 1
indprof <- 1
curve(prof_indSN,-0.4,0 , n = 200, lwd = 2, xlab = expression(mu), ylab = "Profile Likelihood",
      cex.axis = 1.5, cex.lab = 1.5)

# Profile likelihood of Parameter 2
indprof <- 2
curve(prof_indlSN,0.75,2 , n = 200, lwd = 2, xlab = expression(sigma), ylab = "Profile Likelihood",
      cex.axis = 1.5, cex.lab = 1.5)

# Profile likelihood of Parameter 1
indprof <- 3
curve(prof_indSN,0, 100000 , n = 1000, lwd = 2, xlab = expression(lambda), ylab = "Profile Likelihood",
      cex.axis = 1.5, cex.lab = 1.5)
