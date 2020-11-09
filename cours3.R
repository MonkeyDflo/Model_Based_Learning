# cours 3 

# input : x
x=as.matrix(iris[,1:4])

# package
library(bayess)
library(mvtnorm)


graphics.off()
par(mfrow=c(3,3))

gmmclustering = function (x,K,itermax=200,init='random'){
  n=nrow(x);p=ncol(x)
  # initialisation des objets
  prop=matrix(NA,itermax+1,K)
  mu=array(NA,dim=c(itermax+1,K,p))
  sigma=array(NA,dim=c(itermax+1,K,p,p))
  loglik=rep(0,itermax+1)
  
  # initialisation de l'ago
  if (init =='random'){
    prop[1,]=rdirichlet(1,par=rep(1,K))
    #mu[1,,]=matrix(rnorm(K*p,mean = mean(colMeans(x)),sd=sqrt(max(diag(var(x))))),K,p)
    mu[1,,]=x[sample(1:n,K),]
    for (k in 1:K) sigma[1,k,,]=rWishart(1, 4, var(x))
  }
  if (init =='kmeans'){
    z=kmeans(x,K)$cluster
    for (k in 1:K){
      prop[1,k]=mean(z==k)
      mu[1,k,]=colMeans(x[which(z==k),])
      sigma[1,k,,]=var(x[which(z==k),])
    }
  }
  
  #calcul de loglik
  for (i in 1:n){
    tmp=0
    for (k in 1:K){
      tmp = tmp + prop[1,k] * dmvnorm(x[i,],mean=mu[1,k,],sigma=sigma[1,k,,])
      # => pk * fk(x) avec f la loi gaussienne.
    }
    loglik[1]=loglik[1] + log(tmp)
  }
  
  # algo EM
  for (iter in 1:itermax){
    #E step
    tik=matrix(NA,n,K) # probabilité de chaque observation d'appartenir à la classe K 
    for (k in 1:K){
      #proportion * la gaussienne multivariée 
      tik[,k] = prop[iter,k] * dmvnorm(x,mu[iter,k,],sigma[iter,k,,])
      # pas plutôt ? dmvnorm(x[i,], .... ) au lieu de dmvnorm de x ? 
      # ah non par ce qu'on recherche sur tout le data set. 
      # on fait une gaussienne sur tout le data set. 
    }
    tik=tik/rowSums(tik) # pourquoi on normalise tik ? 
    
    #M step
    for (k in 1:K){
      nk=sum(tik[,k]) #somme de toutes les probabilités des observations d'appartenir 
      #à la classe k ! 
      # = log fonction de vraisemblance = P(X=x1) + ... + P(X=xn) ?
      prop[iter+1,k]= nk/n 
      #pour avoir la proportion de point dans la classe 
      #on divise la somme nk par le nombre d'observations/le nombre de points n
      mu[iter+1,k,]=colSums(tik[,k]*x) / nk
      #On prend toutes les probabilités des obs n d'appartenir à la classe k
      # pourquoi on multiplie par x ? sorte de moyenne pondérée par la proba d'appartenance
      # on fait la somme 
      # et on divise par la somme de toutes les probabilités d'appartenir à k
      
      #sigma[iter+1,k,,] <- lapply(1:n, function(m) tik[m,k]*(X[m,]-mu[iter+1,k,])%*%t(X[m,]-mu[iter+1,k,]))/nk
      
      # sigma[iter+1,k,,] <- lapply(1:n, 
      #                             function(m){
      #                               tik[m,k] * (X[m,]-mu[iter+1,k,]) %*% t(X[m,]-mu[iter+1,k,])
      #                               }
      #                             ) / nk
      
      sigma[iter+1,k,,] = lapply(X = 1:n, 
                                 FUN = function(rowId) tik[rowId,k] * (x[rowId,] - mu[iter+1,k,]) %*% t(x[rowId,] - mu[iter+1,k,])
                                 ) /nk
      
      # on recalcule sigma pour chaque classe.
      
    }
    
    #calcul de loglik
    #On recalcule la log vraisemblance
    #maintenant que l'on a un nouveau theta
    for (i in 1:n){
      tmp=0
      for (k in 1:K){
        tmp= tmp + prop[iter+1,k] * dmvnorm(x[i,],mean=mu[iter+1,k,],sigma=sigma[iter+1,k,,])
      }
      loglik[iter+1]=loglik[iter+1] + log(tmp)
    }
    
    z=max.col(tik)
    
    if ((iter-1) %% 5 == 0){ #condition de fin
      #Sys.sleep(0.5)
      plot(x[,1:2],col=z,main=paste('iteration :',iter,', loglik =',round(loglik[iter+1],2)),cex.main=0.8)
    }
    #if(loglik[iter+1]<loglik[iter]) browser()
  }
  return(list(prop=prop,mu=mu,sigma=sigma,K=K,loglik=loglik,z=z))
}

res=gmmclustering(x,2,itermax = 20,init='random')
#par(mfrow=c(2,2))
print(res$loglik)
plot(res$loglik,type='l',main=paste('max loglik :',max(res$loglik)),cex.main=0.8)
plot(x[,1:2],col=res$z,main='final partition')
#plot(x,col=kmeans(x,3)$cluster)

#Plot des mu
for (j in 1:ncol(x)){
  for (k in 1:2){
    if (j*k==1) plot(res$mu[,k,j],ylim=c(min(res$mu),max(res$mu)),type='l',main="evolution des mu")
    else lines(res$mu[,k,j])
  }
}
#Plot de prop
for (k in 1:2){
  if (k==1) plot(res$prop[,k],ylim=c(0,1),type='l',main="evolution des prop")
  else lines(res$prop[,k])
}

#BIC Criterion ####
# theta_^ = all parameters 
# l = log lik
# v = number of parameters
# n = number of parameter in the data set
# BIC = loglik + (v/2) * ln(n)
v = length(res$prop) + length(res$mu) + length(res$sigma)
BIC = res$loglik + (v/2) * ln(nrow(x))

vectorBIC = array(NA, 5)
for( k in c(2,3,4,5)){
  resTemp = gmmclustering(x,k,itermax = 20,init='random')
  vTemp = length(resTemp$prop) + length(resTemp$mu) + length(resTemp$sigma)
  vectorBIC[k] = resTemp$loglik + (vTemp/2) * ln(nrow(x))
}
