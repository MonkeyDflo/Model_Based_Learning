#EM algo classes latentes
#20201111
#Florent Jakubowski

#packages ####
library(bayess)
library(mvtnorm)

#fonction ####
EMAlgoLatentClassModel = function(x, K = 2, ITERMAX = 10, threshold = 1e-6){
# instanciation ####
# n nombre d'individus/éléments dans x
n = nrow(x)
# p nombre de features/variables catégorielles 
p = ncol(x)
# mj nombre de catégorie par features
mj = vector(mode = "list", length = p)
for(i in 1:p){
  #mj[i] = unique(x[,i])
  mj[i] = list(unique(x[,i]))
}
#pour accéder à un élément : print(mj[[10]][3])
#pour connaître le nb de catégories par variables catégorielles : print(length(mj[[10]]))
prop = rep(NA, K)
alpha = array( data = list(), dim = c(k,p))
for(k in 1:K){
  for(j in 1:p){
    alpha[k,j] = list(rep(NA, length(mj[[j]]) ) ) 
  }
}
print(alpha)
#loglik 
# initialisation ####
# EM Algo ####
# E Step ####
# M step ####
  
}

#test ####
x = read.csv2("/home/florent/Documents/Model Based Learning/datas.csv")[,2:11]
EMAlgoLatentClassModel(x)
