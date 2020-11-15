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

# function loglik ####
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