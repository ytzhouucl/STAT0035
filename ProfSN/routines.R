# negative log-likelihood function
loglikSN <- function(par){
  mu <- par[1]
  sigma <- exp(par[2])
  lambda <- par[3]
  
  loglik <- sum( dsn( frontier, xi = mu, omega = sigma, alpha =lambda, log = TRUE) )
  
  return(-loglik)
  
}



# Profile likelihood function for parameter "ind"
prof.likSN <- function(par1, ind){
  
  tempf <- function(par){
    tempv <- rep(0,p)
    tempv <- replace(x = tempv, c(1:p)[-ind] , par)
    tempv <- replace(x = tempv, ind , par1)
    out0 <- loglikSN(tempv)
    return(out0)
  } 
  
  out <-  -nlminb(OPTSN$par[-ind],tempf, control = list(iter.max = 10000))$objective + ML
  
  return(exp(out))
}


prof_indSN <- Vectorize(function(par) prof.likSN(par,indprof))
prof_indlSN <- Vectorize(function(par) prof.likSN(log(par),indprof))
