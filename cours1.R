#Model Based Learning
#Cours 1 
#Florent Jakubowski
#20201027


#The Gaussian Mixture Model ####
x=seq(-10,10,.1)
plot(x,dnorm(x,-2,1),type='l',col=1,ylab='',ylim=c(0,0.4))
lines(x,dnorm(x,2,2),col=2)
lines(x,.5*dnorm(x,-2,1)+.5*dnorm(x,2,2),col=3)
legend('topright',legend=c('comp. 1','comp. 2','mixture'),
       col=1:3,lty=1)

?dnorm
#Normal Distibution 
# Density, distribution function, quantile function and random generation 
# for the normal distribution with mean equal to mean and standard deviation equal to sd.
dnorm(x,-2,1)

#Mixture model estimation in classification ####
#Mixture Model estimation in classification
# Maximum likelihood estimation

#Exercice 1 ####

#Application on iris data set ####
set.seed(2)
tmp=sample(1:150,50) #génère 50 données entre un et 150. 
X.train <- iris[-tmp,-5]
Class.train <- iris[-tmp,5]
X.test <- iris[tmp,-5]
Class.test <- iris[tmp,5]
#Estimation of gaussian mixture 
irisMclustDA <- MclustDA(X.train, Class.train,
                         modelNames = "XXX")
summary(irisMclustDA)
#evaluation of prediction
tmp=summary(irisMclustDA, newdata = X.test,
            newclass = Class.test)
print(tmp$tab.newdata)
#Mixture of mixture of iris data set ####
irisMclustDA <- MclustDA(X.train, Class.train,
                         modelType = "MclustDA")
summary(irisMclustDA)
#evaluation of the prediction 
tmp=summary(irisMclustDA, newdata = X.test,
            newclass = Class.test)
print(tmp$tab.newdata)
#Exercice 2 ####
# Implement your own maximum likelihood estimation for a Gaussian
# mixture model.
# Test it on simulated data.

#input : (x1, y1), ..., (xn, yn)
# dnv norm = densité gaussienne multivariée = fk en un point x multivariée
#d'espérance k et ... k
#output la classe prédite

#Correction Exo 2####
# input : x,z
train=sample(1:150,100)
x=iris[train,1:4]
z=iris[train,5]

gmmclassif = function (x,z){
  K=length(levels(z)) #nombre de classe
  n=nrow(x);p=ncol(x) #n : nombre d'échantillons, p : dimensions
  prop=rep(NA,K) # vecteur de 3 données NA, NA, ...
  mu=matrix(NA,K,p) # matrice de 3 * 4 remplie de NA
  sigma=array(NA,dim=c(K,p,p)) # 3 * 4 * 4
  for (k in 1:K){
    prop[k]=mean(z==levels(z)[k])
    mu[k,]=colMeans(x[which(z==levels(z)[k]),])
    sigma[k,,]=var(x[which(z==levels(z)[k]),])
  }
  return(list(prop=prop,mu=mu,sigma=sigma,K=K))
}

library(mclust)

predict.gmmclassif = function (mod,x){
  tik=matrix(NA,nrow(x),mod$K)
  for (k in 1:mod$K){
    tik[,k] = mod$prop[k] * dmvnorm(x,mod$mu[k,],mod$sigma[k,,])
  }
  tik=tik/rowSums(tik)
  z=max.col(tik)
  return(list(z=z,tik=tik))
}

mod=gmmclassif(x,z)
pred=predict.gmmclassif(mod,iris[-train,1:4])
table(pred$z,iris[-train,5])




#Mixture model estimation in clustering ####
# !!!!! important #
#The EM algorithm ####
#Exercice 3 ####
# Implement your own EM algorithm for Gaussian mixture model
# estimation.
# Test it on simulated data.

Ex3 <- function(){
  #input : x
  #theta : repartition p, mu, matrice de variance covariance. 
  
  #output: 
}

rm(list = ls())
train=sample(1:150, 100) #génère 100 données de 1 à 150.
x=iris[train,1:4] # prend 100 valeurs au hasard du data set iris avec les 
z=iris[train,5] # prend les classes correspondantes des x choisis.

emGMMclassif = function(){

}


#Classification EM ####
#Stochastic EM ####
#Exercice 4####
#Implémentez votre propres algorithmes des Kmeans
