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
      tmpLoglik = tmpLoglik + prop[k] * dmvnorm(x[i,],mean=mu[k],sigma=sigma[k,,])
    }
    loglik[1] = loglik[1] + ln(tmpLoglik)
  }
  
  # # algo EM ####
  # for(iter in 1:ITERMAX){
  #   tik = matrix(data = rep(NA, n*KnbClasse), nrow = n, ncol = knbClasse) 
  #   # E STEP ####
  #   for(k in 1:KnbClasse){
  #     for(i in 1:n){
  #       tik[i,k] = dmvnorm(x[i,], mean = mu[k], sigma = sigma[k,,])*prop[k]
  #     }
  #     tik[,k] = tik[,k] / sum(tik[,k])
  #   }
  #   # M STEP ####
  #   for(k in 1:KnbClasse){
  #     nk = sum(tik[,k])
  #     # pk = nk/n avec nk = somme de i à n des tk(xi)
  #     prop[k] = nk / n
  #     # muk = (1/nk)*somme de i jusqu'à n des tk(xi)*xi
  #     sommeTikxi = rep(NA, p)
  #     for(i in 1:n){
  #       sommeTikxi = sommeTikxi + tik[i,k]*x[i,]
  #     }
  #     mu[k] = sommeTikxi / nk
  #     # sigmak = (1/nk)*somme de i jusqu'à n des tk(xi)*(xi - muk)transpose *(xi-muk)
  #     sigma[k,,] = Reduce('+', lapply(1:n, function(m){
  #       tik[m,k] * (x[m,]-mu[k]) %*% t(x[m,]-mu[k])
  #     }))/nk
  #     #
  #     # sigma[iter+1,k,,] <- Reduce('+',lapply(1:n, function(m){
  #     #   tik[m,k]*(x[m,]-mu[iter+1,k,])%*%t(x[m,]-mu[iter+1,k,])/nk}))
  #   }
  #   # Calcul de la logvraisemblance ####
  #   for(i in 1:n){
  #     tmpLoglik = 0
  #     for(k in 1:KnbClasse){
  #       tmpLoglik = tmpLoglik + prop[k] * dmvnorm(x[i,], mean = mu[k],  sigma = sigma[k,,])
  #     }
  #     loglik[iter] = loglik[iter] + ln(tmpLoglik)
  #   }
  #   # z ####
  #   z=max.col(tik)
  #   
  # }
  # return(list(prop=prop,mu=mu,sigma=sigma,KnbClass=KnbClass,loglik=loglik,z=z))
}

#Test

x = as.matrix(iris[,(1:4)])
var(x)
varcovar = rWishart(1, ncol(x), var(x))

dmvnorm(x[1,], mean = 5L, sigma = varcovar)
length(x[1,])
length(varcovar)


EMAlgo(x, 2, 10)

