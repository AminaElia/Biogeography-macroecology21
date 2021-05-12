setwd("/users/Amina/Desktop/biogeo")
dat <- read.csv("data_biomac_3.csv")
library(vegan)
dim(dat)
com <- dat[, -(1:7)] #creo la community togliendo le prime 7 colonne contenendo info che non son le specie
rowSums(com > 0)
hist(specnumber(com))
elba <- dat[dat$island == "Elba", ] #attenzione questa non funzia...
sum(specnumber(com, MARGIN = 2) == 1) # number of species occurring only once
head(sort(specnumber(com, MARGIN = 2), decreasing = T), 15) # first 15 species with higher freq
specnumber(com, groups = dat$island) #gamma diversity between islands
specnumber(com, groups = dat$habitat) #gamma diversity between habitats

