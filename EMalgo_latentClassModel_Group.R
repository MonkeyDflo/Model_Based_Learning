#EM Algo latent class Model 
#copyright Khanh, Marc, Thomas, Quentin, Cintia, Khaled, Vincent, Jairo, JJ, Florent

vector2binary2 <- function (x) {
  xunique <- unique(x)
  y <- matrix(0, nrow=length(x), ncol=length(xunique))
  for (i in 1:length(x)) {
    y[i, which(xunique == x[i])] <- 1
  }
  return(y)
}

LatentClassModel = function(x, K, ITERMAX, threshold = 1e-6){
  # instanciation ####
  # x to Binary matrice ####
  xBin <- lapply(x, vector2binary2)
  xBinary <- Reduce(cbind, xBin)
  Catégories <- 
  
  
  mNbCatégories = ncol(xBinary)
  pNbObs = nrow(xBinary)
  # proportions
  prop = rep(NA, KnbClasse)
  # alpha k 
  alpha = array(data = rep(NA, (KnbClasse*pNbObs*mNbFeatures)) , dim = c(KnbClasse, pNbObs, mNbFeatures))
  # loglik
  loglik = rep(NA, ITERMAX+1)
  
  # initialisation ####
  # Algo EM ####
  # E step ####
  # M step ####
}

# Test ####
x = read.csv2("/home/florent/Documents/Model Based Learning/datas.csv")[,2:11]
xBin <- lapply(x, vector2binary2)
xBinary <- Reduce(cbind, xBin)

