#cours 4
#09112020

library(MBCbook)
data(PoliticalBlogs)
head(PoliticalBlogsData)


library(mixer)
fitb <- mixer(xSampson , qmin=1, qmax=5,method="variational",
              verbose = F)
mod <- getModel(fitb)
z <- t(mod$Taus)
plot(samplike, label=1:18, mode="fruchtermanreingold",
     coord=layout,vertex.col=max.col(z)+1)


# correction ####
