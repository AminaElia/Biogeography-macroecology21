#components of Beta-diversity
setwd
library(vegan)
library(betapart)
data(BCI)
str(BCI) #str sta per structure of the dataframe
sr <- specnumber(BCI) #species richness
# attencion! prima il numero della row e sptto l'actual number of sr
hist(sr, xlab = "Species richness", ylab = "nu,ber of plots", main = NULL)
summary(sr)
BCI_pa <- decostand(BCI, method = "pa")
vegdist(BCI_pa [1:2, ], method = "jaccard") #to calculate the jaccard distance between the first two sites
vegdist(BCI_pa[1:3, ], method = "jaccard") # distance tra i primi tre dites
d1 <- vegdist(BCI_pa, method = "jaccard")
hist(d1)
summary(d1)
beta.multi(BCI_pa, index.family = "jaccard") # first is turnover, second is nestedness, third is jaccard as total beta-diversity
#turnover is much bigger so is dominant. dissimilarity!!
d2 <- beta.pair(BCI_pa, index.family = "jaccard")
boxplot(d2, names = c("Turnover", "Nestedness", "Total beta")) #50% of obseervation are inside the box
