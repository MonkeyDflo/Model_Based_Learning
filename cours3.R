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
      tmp=tmp + prop[1,k] * dmvnorm(x[i,],mean=mu[1,k,],sigma=sigma[1,k,,])
    }
    loglik[1]=loglik[1] + log(tmp)
  }
  
  # algo EM
  for (iter in 1:itermax){
    #E step
    tik=matrix(NA,n,K)
    for (k in 1:K){
      tik[,k] = prop[iter,k] * dmvnorm(x,mu[iter,k,],sigma[iter,k,,])
    }
    tik=tik/rowSums(tik)
    
    #M step
    for (k in 1:K){
      nk=sum(tik[,k])
      prop[iter+1,k]= nk/n
      mu[iter+1,k,]=colSums(tik[,k]*x) / nk
      
      #browser()
      
      # sigma[iter+1,k,,]= t(tik[,k] *(x-mu[iter+1,k,])) %*% (x-mu[iter+1,k,]) / nk
      #  sigma[iter+1,k,,]=( sigma[iter+1,k,,] + t(sigma[iter+1,k,,]) )/2       
      
      sigma[iter+1,k,,] <- lapply(1:n, function(m) tik[m,k]*(X[m,]-mu[iter+1,k,])%*%t(X[m,]-mu[iter+1,k,]))/nk
      
      
    }
    
    #calcul de loglik
    for (i in 1:n){
      tmp=0
      for (k in 1:K){
        tmp= tmp + prop[iter+1,k] * dmvnorm(x[i,],mean=mu[iter+1,k,],sigma=sigma[iter+1,k,,])
      }
      loglik[iter+1]=loglik[iter+1] + log(tmp)
    }
    
    z=max.col(tik)
    
    if ((iter-1) %% 5 == 0){
      #Sys.sleep(0.5)
      plot(x[,1:2],col=z,main=paste('iteration :',iter,', loglik =',round(loglik[iter+1],2)),cex.main=0.8)
    }
    #if(loglik[iter+1]<loglik[iter]) browser()
  }
  return(list(prop=prop,mu=mu,sigma=sigma,K=K,loglik=loglik,z=z))
}

res=gmmclustering(x,2,itermax = 20,init='kmeans')
#par(mfrow=c(2,2))
print(res$loglik)
plot(res$loglik,type='l',main=paste('max loglik :',max(res$loglik)),cex.main=0.8)
plot(x[,1:2],col=res$z,main='final partition')
#plot(x,col=kmeans(x,3)$cluster)

#BIC Criterion ####
# theta_^ = all parameters 
# l = vraisemblance
# mu = number of parameters
# n ? 
# log(l(theta_^,x)) + (mu/2) * ln(n)

for (j in 1:ncol(x)){
  for (k in 1:2){
    if (j*k==1) plot(res$mu[,k,j],ylim=c(min(res$mu),max(res$mu)),type='l',main="evolution des mu")
    else lines(res$mu[,k,j])
  }
}
for (k in 1:2){
  if (k==1) plot(res$prop[,k],ylim=c(0,1),type='l',main="evolution des prop")
  else lines(res$prop[,k])
}