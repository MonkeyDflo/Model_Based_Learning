##EMalgo_latentClass_myVersion

# packages ####
library(bayess)
library(mvtnorm)

# fonction ####
EMalgoLatentClassModel = function(x, KnbClasse, ITERMAX, threshold = 10e-6){
  # instanciation ####
  mNbFeatures = ncol(x)
  pNbObs = nrow(x)
  # proportions
  prop = rep(NA, KnbClasse)
  # alpha k 
  alpha = array(data = rep(NA, (KnbClasse*pNbObs*mNbFeatures)) , dim = c(KnbClasse, pNbObs, mNbFeatures))
  # loglik
  loglik = rep(NA, ITERMAX+1)
  
  # initialisation ####
  # prop
  prop = rdirichlet(1, par = rep(1, KnbClasse))
  # alpha 
  for(k in 1:KnbClasse){
    for(j in 1:pNbObs){
        alpha[k,j,] = rdirichlet(1, par = rep(1, mNbFeatures))
    }
  }
  # loglik
  for(k in KnbClasse){
    PrdObs = 1
    for(j in pNbObs){
      PrdFeatures = 1
      for(i in mNbFeatures){
        PrdFeatures = PrdFeatures * alpha[k,j,i]
      }
      PrdObs = PrdObs * PrdFeatures
    }
    loglik[1] = loglik[1] + log(PrdObs)
  }
  
  
  #EM Algo ####
  for(iter in 1:ITERMAX){
    tk = rep(1, KnbClasse)
    # E step ####
    for(k in 1:KnbClasse){
      tmp = 1
      for(j in 1:pNbObs){
        tmp = tmp * prod(alpha[k,j,])
        # print(prod(alpha[k,j,]))
        # print(paste("j : ",j, " tmp : ",tmp))
      }
      tk[k] = tk[k] + (prop[k] * tmp)
      # print(paste("k : ",k, " tk[k] : ",tk[k]))
    }
    # print(paste("iter : ",iter, " tk : ",tk))
    
    sumTk = sum(tk)
    for(k in KnbClasse){
      tk[k] = tk[k] / sumTk
      # print(paste("k : ",k, " tk : ",tk))
    }
    # print(paste("iter : ",iter, " tk : ",tk))
    
    
    
    
    # M step ####
  }
  
}

# tests ####
# tests instanciation ####
nbRows <- 10 # sample size
nbFeatures <- 4
x = matrix(data = rep(NA, nbRows*nbFeatures), nrow = nbRows, ncol = nbFeatures)
for(i in 1:nbRows){
  x[i,] = rep(0, nbFeatures)
  x[i,sample(1:nbFeatures, 1)] = 1
}
EMalgoLatentClassModel(x, 2, 10)

