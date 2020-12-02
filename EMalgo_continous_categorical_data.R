#Description ####
#EM Algorithm for continous variables and ctegorical variables
#Model Based Learning project
#Groupe : Florent Jakubowski et Marc Telly 
#Date : 2020-11-15
#For mor informations read the README
#All functions should be tested with unit tests using testthat package
#To understand our way of writting code see the book CleanCode. 

# packages ####
library(bayess)
library(mvtnorm)


# function TransformCategoricalVectorToBinaryMatrice ####
TransformCategoricalVectorToBinaryMatrice <- function (categoricalVector) {
  vectorUniqueValues <- unique(categoricalVector)
  binaryMatrice <- matrix(0, nrow=length(categoricalVector), ncol=length(vectorUniqueValues))
  for (i in 1:length(categoricalVector)) {
    binaryMatrice[i, which(vectorUniqueValues == categoricalVector[i])] <- 1
  }
  return(binaryMatrice)
}

# function BinarizeCategorialMatrice #####
BinarizeCategoricalMatrice = function(CategoricalMatrice) {
  if( ncol(CategoricalMatrice) == 1){ 
    return(TransformCategoricalVectorToBinaryMatrice(CategoricalMatrice))
    }
  else{
    BinarizedCategoricalMatrice = Reduce(cbind, lapply(CategoricalMatrice, TransformCategoricalVectorToBinaryMatrice))
    return(BinarizedCategoricalMatrice)
  }
}

# function CutDataSetIntoCategoricalandContinous ####
SplitDataSetIntoCateAndContiMatrices = function(x){
  ContinousMatrix = c()
  CategoricalMatrix = c()
  for(j in 1:ncol(x)){
    if(is.numeric(x[,j])){
      ContinousMatrix = cbind(ContinousMatrix, x[,j])
    }
    if(is.factor(x[,j])){
      CategoricalMatrix = cbind(CategoricalMatrix, x[,j])
    }
  }
  p = 
  return(list(ContinousMatrix = ContinousMatrix,
              CategoricalMatrix = CategoricalMatrix,
              p = p))
}

# function getP ####
getP = function(x){
  for(j in 1:ncol(x)){
    if(is.factor(x[,j])){
      p = p + 1
    }
    return(p)
  }
}

ds_test = iris
res = SplitDataSetIntoCateAndContiMatrices(ds_test)
x = cbind(res$CategoricalMatrix, res$ContinuousMatrix)
x = getP(x)
# TransformCategoricalVectorToBinaryMatrice(res$CategoricalMatrix)
# xBinary = BinarizeCategoricalMatrice(res$CategoricalMatrix)
# print(xBinary)

# function loglik ####
# on suppose que les données en entrée présentes d'abord des données catégorielles binarisées
# puis des données continues. 
CalculateLoglik = function(x, KnbClasse, prop, alpha){
  n = nrow(x)
  nRowVarCaté 
  nRowVarContinuous
  SommeSurIndividus = 0
  for(i in 1:n){
    SommeSurClasse = 0
    for(k in 1:KnbClasse){
      ProduitSurCateg = 1
      for(j in 1:length(alpha[1,])){
        #si x compris dans l'intervalle alors fk = dmvnorm 
        #sinon fk = 1
        if(i == ){
          
        }
        else{
          
        }
        #ProduitSurCateg = ProduitSurCateg * alpha[k,j]^x[i,j]
      }
      SommeSurClasse = SommeSurClasse + prop[k] * ProduitSurVarCateg
    }
    SommeSurIndividus = SommeSurIndividus + log(SommeSurClasse)
  }
  
}
# function Instanciate ####
# function Initialize ####
# function printSomeUsefulGraphics ####
# function DetermineGMMclustForCategoricalContinousData ####
# function EstimateLCMforCategoricalContinousDatas ####
# EM algorithm for estimating the latent class model
# for mixed data
EstimateLCMforCategoricalContinousDatas = function(x, KnbClasse = 2, ITERMAX = 10, THRESHOLD = 1e-6){
  #Instanciation ####
  # xBinary : Matrice de données catégorielles codées en vecteur binaire
  # xContinous : Matrice regroupant les colonnes de variables continues
  # xBinaryandContinous : Matrice regroupant xBinary et xContinous à la suite
  # indiceC : colonne c où s'arrête les données catégorielles et où commence les données continues
  # loglik : vecteur de longueur ITERMAX
  # prop : vecteur de longueur KnbClasse
  # alpha : matrice de dimension row = nrow(x) et col = ncol(xBinary) ? ou ncol(xBinaryandContinous) ?
  # mu : vecteur de longueur KnbClasse
  # sigma : Matrice des matrice de variances covariances entre les variables continues ? ou toutes les variables ? 
  # sigma a pour dimension dim = c(K,ncol(xContinous),ncol(xContinous))
  #Initialization ####
  #prop
  #alpha
  #mu
  #sigma
  #loglik : formule = 
  #EM Algorithm ####
  #E Step ####
  #instanciation des tik : dim = c(k,nrow(x))
  #calcule des tik : formule = 
  #M Step ####
  #instanciation de nk
  #calcul de nk
  #actualisation des paramètres prop, alpha, mu, sigma
  #prop
  #alpha
  #mu
  #sigma
  #loglik ####
  #return ####
}

#tests####

