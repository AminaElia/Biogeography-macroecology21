setwd("/users/Amina/Desktop/biogeo")
dat <- read.csv("data_biomac_3.csv")
library(vegan)
dim(dat)
com <- dat[, -(1:7)]
rowSums(com > 0)
hist(specnumber(com))
elba <- dat[dat$island == "Elba", ]
sum(specnumber(com, MARGIN = 2) == 1)
head(sort(specnumber(com, MARGIN = 2), decreasing = T), 15)
