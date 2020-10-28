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

#Mixture model estimation in clustering ####
#The EM algorithm ####
#Exercice 3 ####
# Implement your own EM algorithm for Gaussian mixture model
# estimation.
# Test it on simulated data.



#Classification EM ####
#Stochastic EM ####
#Exercice 4####
#Implémentez votre propres algorithmes des Kmeans
