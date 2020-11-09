#EM algorithm
rm(list = ls())

EM = function(X, KnbClasses){
  #on a besoin de theta pour chaque cluster 
  #theta et égale à : p la proportion, mu la moyenne , sigma² la matrice de variance covariance.
  #On définit un nombre d'itération max
  XnbRows = nrows(X);
  XnbCols = ncols(X);
  ITERMAX = 3;
  propors = matrix(data = rep(NA,(KnbClasses*ITERMAX)), ncol=KnbClasses, nrow =ITERMAX)
  moyennes = matrix(data = rep(NA,(KnbClasses*ITERMAX)), ncol=KnbClasses, nrow =ITERMAX)
  matVarCovar = array(data = NA, dim=c(ITERMAX,KnbClasses,XnbCols,XnbCols))
  #Loglikelihood = matrix(data = rep(NA,(KnbClasses*ITERMAX)), ncol=KnbClasses, nrow =ITERMAX)
  # theta* = argmax du loglik
  #intialisation ####
  propors[1,] = runif(n=2, min=1, max=3)
  moyennes[1,] =  
  matVarCovar[1,,XnbCols,XnbCols]=
    
  # boucle for ITERMAX
  # boucle for Knb classse : pour chaque classe 
    #faire la log vraisemblance avec les données 
    #faire la argmax de la logvraisemblance pour obtenir theta*
    #modifier les paramêtres de la gaussienne avec les valeurs de theta*
  for(i in 1:KnbClasses){
    #il faut initialiser theta les paramètres de chaque gaussienne
    print(propors[,i])
  }
  
  
  #E step ####
  #M step ####
  #BIC Criterion ####
  # theta_^ = all parameters 
  # l = vraisemblance
  # mu = number of parameters
  # n ? 
  # log(l(theta_^,x)) + (mu/2) * ln(n)

  #return ####
  return(propors)
}

#test
X = matrix(data=seq(1:9), ncol=3 , nrow=3)
proporsRes = EM(X, 2)

seq(1:9)
rep(NA, 10)

X[1,]