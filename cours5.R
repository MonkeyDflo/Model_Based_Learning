# cours 5
# author : Florent Jakubowski
# 12/10/2020
# Notions abordées : coclustering, stochastc bloc model 
# packages utilisés : blockcluster, ordinal clust 
# data set : young people survey 
# disponible à cette adresse :www.kaggle.com/miroslavsabo/young-people-survey

rm(list = ls())

library(blockcluster)
data(binarydata)
out<-coclusterBinary(binarydata,nbcocluster=c(2,3))
summary(out)
plot(out)

library(ordinalClust)
columns = read.csv2("/home/florent/Documents/Model Based Learning/YoungPeopleSurvey/columns.csv")
responses = read.csv2("/home/florent/Documents/Model Based Learning/YoungPeopleSurvey/responses.csv", sep = ",")
set.seed(5)

dropNonOrdinalVariablesofLength5 = function(responses){
  ncol(responses)
  indices = c()
  for(i in 1:ncol(responses)){
    currModals = length(unique(responses[,i]))
    lengthCurrModals = length(unique(responses[,i]))
    if(
      ( lengthCurrModals != 6 )
       &&( lengthCurrModals != 5 )
       )
      {
      #print(currModals)
      indices = append(indices, i)
      #print(indices)
      print(unique(responses[,i]))
    } 
  }
  for(i in 1:length(indices)){
    responses = responses[,-indices[i]]
  }
  print(ncol(responses))
  return(responses)
}

responses = dropNonOrdinalVariablesofLength5(responses)

#code cintia 
df2 <- df %>% select_if(is.numeric)
df2 <- df2[,1:134]

# loading the ordinal data
M <- as.matrix(responses)
typeof(as.double(M))
typeof(responses)
typeof(as.double(responses))
# defining different number of categories:
m=5
# defining number of row and column clusters
krow = 5
kcol = 4
# configuration for the inference
nbSEM=50
nbSEMburn=40
nbindmini=2
init = "kmeans"

# Co-clustering execution
coclust <- boscoclust(x=M,
                      kr=krow,
                      kc=kcol,
                      m=m,
                      nbSEM=nbSEM,
                      nbSEMburn=nbSEMburn, 
                      nbindmini=nbindmini, 
                      init=init)
plot(coclust)

