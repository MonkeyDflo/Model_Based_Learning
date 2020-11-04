#EM algorithm
rm(list = ls())

EM = function(X, KnbClasses){
  #on a besoin de theta pour chaque cluster 
  #theta et égale à : p la proportion, mu la moyenne , sigma² la matrice de variance covariance.
  #On définit un nombre d'itération max
  NbRows = nrows(X);
  NbCols = ncols(X);
  ITERMAX = 3;
  propors = matrix(rep(NA,(KnbClasses*ITERMAX)), ncol=KnbClasses, nrow =ITERMAX)
  moyennes = matrix(rep(NA,(KnbClasses*ITERMAX)), ncol=KnbClasses, nrow =ITERMAX)
  matVarCovar = matrix(rep(NA,(KnbClasses*ITERMAX)), ncol=KnbClasses, nrow =ITERMAX)
  sigma=array(data = NA, dim=c(ITERMAX,KnbClasses,p,p))
  #intialisation ####
  propors[1,] = runif(n=2, min=1, max=3)
  
  for(i in 1:KnbClasses){
    #il faut initialiser theta les paramètres de chaque gaussienne
    print(propors[,i])
  }
  #E step ####
  #M step ####
  #return ####
  return(propors)
}

#test
X = matrix(data=seq(1:9), ncol=3 , nrow=3)
proporsRes = EM(X, 2)

seq(1:9)
rep(NA, 10)

X[1,]