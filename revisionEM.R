#EM algorithm
rm(list = ls())

library(bayess)
library(mvtnorm)

EMAlgo <- function(x, KnbClasse, ITERMAX, tolerance = 10e-6){
  #instanciation ####
  n = nrow(x)
  p = ncol(x)
  prop = array(data = rep(NA, KnbClasse), dim=KnbClasse)
  mu = array(data = rep(NA, KnbClasse), dim=KnbClasse)
  sigma = array(data = NA, dim=c(KnbClasse,p,p))
  loglik = rep(0, ITERMAX+1)
  #initialisation ####
  prop = rdirichlet(1, par = rep(1, KnbClasse))
  mu = x[sample(1:n, KnbClasse)];
  for(k in 1:KnbClasse) sigma[k,,] = rWishart(1, p, var(x))
  for(i in 1:n){
    tmpLoglik = 0
    for(k in 1:KnbClasse){
      tmpLoglik = tmpLoglik + prop[k] * dmvnorm(x[i,], mean = mu[k],  sigma = sigma[k,,])
    }
    loglik[1] = loglik[1] + ln(tmpLoglik)
  }
  
  # algo EM ####
  for(iter in 1:ITERMAX){
    tik = matrix(data = rep(NA, n*KnbClasse), nrow = n, ncol = knbClasse) 
    # E STEP ####
    for(k in 1:KnbClasses){
      for(i in 1:n){
        tik[i,k] = dmvnorm(x[i,], mean = mu[k], sigma = sigma[k,,])*prop[k]
      }
      tik[,k] = tik[,k] / sum(tik[,k])
    }
    # M STEP ####
    # implémenter 3 formules 
    
  }
  
  
}

#Test

x = as.matrix(iris[,(1:4)])
var(x)
EMAlgo(x, 2, 10)

