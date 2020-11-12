#EM Algo latent class Model 
#copyright Khanh, Marc, Thomas, Quentin, Cintia, Khaled, Vincent, Jairo, JJ, Florent

# packages ####
library(bayess)
library(mvtnorm)

# function vector2binary #### 
vector2binary <- function (x) {
  xunique <- unique(x)
  y <- matrix(0, nrow=length(x), ncol=length(xunique))
  for (i in 1:length(x)) {
    y[i, which(xunique == x[i])] <- 1
  }
  return(y)
}


# function Calculateloglik ####
CalculateLoglik = function(KnbClasse, prop, alpha){
  SommeSurClasse = 0
  for(k in 1:KnbClasse){
    ProduitSurCatés = 1
    for(j in 1:length(alpha[1,])){
      ProduitSurCatés = ProduitSurCatés * alpha[k,j]
    }
    SommeSurClasse = SommeSurClasse + prop[k] * ProduitSurCatés
  }
  return(log(SommeSurClasse))
}
# function Instanciate ####
Instanciate = function(x, KnbClasse, ITERMAX ){
  # Nombre de catégories par Variables catégorielles ####
  NbVarCatégorielles = ncol(x)
  VectNbCatégories = rep(NA, NbVarCatégorielles)
  for(p in 1:NbVarCatégorielles){
    VectNbCatégories[p] = length(unique(x[,p]))
  }
  # x to Binary matrice ####
  xBinary = Reduce(cbind, lapply(x, vector2binary))
  mNbCatégories = ncol(xBinary)
  nNbObs = nrow(xBinary)
  # prop, alpha, loglik ####
  # proportions
  prop = rep(NA, KnbClasse)
  # alpha k 
  alpha = array(data = rep(NA, (KnbClasse*mNbCatégories)) , dim = c(KnbClasse, mNbCatégories))
  # loglik
  loglik = rep(NA, ITERMAX+1)
  # return ####
  return(list(xBinary=xBinary,
              NbVarCatégorielles=NbVarCatégorielles,
              VectNbCatégories=VectNbCatégories,
              prop=prop,
              alpha=alpha,
              loglik=loglik))
}
# test Instanciate ####
testInst = Instanciate(x,2,10)
print(testInst)

# function InitializeVars ####
InitializeVars = function(xBinary, 
                          KnbClasse, 
                          ITERMAX, 
                          NbVarCatégorielles,
                          VectNbCatégories, 
                          prop, 
                          alpha, 
                          loglik){
  # initialisation de prop ####
  prop = rdirichlet(1, par = rep(1, KnbClasse))
  # initialisation de alpha ####
  for(k in 1:KnbClasse){
    CurrInd = 1
    for(p in 1:NbVarCatégorielles){
      vectTemp = rdirichlet(1, par = rep(1, VectNbCatégories[p]) )
      alpha[k,CurrInd:(CurrInd + VectNbCatégories[p] - 1)] = vectTemp
      CurrInd = CurrInd + VectNbCatégories[p]
    }
  }
  # initialisation de la loglik #### 
  # à revoir ####
  loglik[1] = CalculateLoglik(KnbClasse, prop, alpha)
  print(loglik[1])
  #return ####
  return(list(prop=prop,
              alpha=alpha,
              loglik=loglik))
}
# test InitializeVars ####
testInst = Instanciate(x,2,10)
testInit = InitializeVars(testInst$xBinary,
                              KnbClasse = 2,
                              ITERMAX = 10,
                              testInst$NbVarCatégorielles,
                              testInst$VectNbCatégories,
                              testInst$prop,
                              testInst$alpha,
                              testInst$loglik)
print(testInit)
print(testInit$alpha)
print(testInit$prop)
# function LatentClass Model ####
LatentClassModel = function(x, KnbClasse = 2, ITERMAX = 10, threshold = 1e-6){
  # instanciation ####
  instVars = Instanciate(x, KnbClasse, ITERMAX)
  # initialisation ####
  initVars = InitializeVars(instVars$xBinary,
                            KnbClasse = 2,
                            ITERMAX = 10,
                            instVars$NbVarCatégorielles,
                            instVars$VectNbCatégories,
                            instVars$prop,
                            instVars$alpha,
                            instVars$loglik)
  
  # Algo EM ####
  n = nrow(x)
  tk = matrix(data = rep(NA, n*KnbClasse), nrow = n, ncol = KnbClasse)
  p = ncol(x)
  for(iter in 1:ITERMAX){
    # E step ####
    for(i in 1:n){
      for(k in 1:KnbClasse){
        prdtmp = 1
        for(j in 1:p){
          
          prdtmph = 1
          for(h in 1:instVars$VectNbCatégories[j]) {
            prdtmph = prdtmph * initVars$alpha[k,(j+h)]^instVars$xBinary[i,(j+h)]
          }
          prdtmp = prdtmp * prdtmph
          
        }
        print(paste("k :",k," & prdtmp : ",prdtmp ))
        tk[i,k] = initVars$prop[k] * prdtmp
      }
      sumtk = sum(tk[i,])
      for(k in KnbClasse){
        tk[i,k] = tk[i,k]/sumtk
      }
    }
    #print(tk)
    # M step ####
    nk = rep(NA, KnbClasse)
    for(k in 1:KnbClasse){
      nk[k] = sum(tk[,k])
    }
    #actualisation de prop
    # formule = pk() = nk/n 
    for(k in 1:KnbClasse){
      initVars$prop[k] = nk[k]/n
    }
    #actualisation de alpha
    # formule : alpha jhk = 1/nk * somme sur n des tik(xi) * x ijh
    for(k in 1:KnbClasse){
      for(j in 1:ncol(instVars$xBinary)){
        sumtkxi = sum(tk[,k]*instVars$xBinary)
        initVars$alpha[k,j] = (1/nk[k]) * sumtkxi
      }
    }
    # loglik ####
    initVars$loglik[iter+1] = CalculateLoglik(KnbClasse, initVars$prop, initVars$alpha)
    print(initVars$loglik[iter+1])
    
  }
  
  # return ####
  return(list(loglik=initVars$loglik))
}

# Test Latent Class Model ####
x = read.csv2("/home/florent/Documents/Model Based Learning/datas.csv")[1:3,2:11]
x = read.csv2("/home/florent/Documents/Model Based Learning/datas.csv")[,2:11]
res = LatentClassModel(x, ITERMAX = 10)

# Note ! Pb avec la loglik ####
print(res$loglik)

plot(res$loglik[1:3])


plot(res$loglik,type='l',main=paste('max loglik :',max(res$loglik)),cex.main=0.8)
plot(x[,1:2],col=res$z,main='final partition')


# test dirichlet ####
a = rep(NA, 4)
a[1:3] = c(0,0,1)

library(bayess)
library(mvtnorm)
sum(rdirichlet(1,par=rep(1,4)))
