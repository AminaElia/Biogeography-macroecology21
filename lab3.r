setwd
library(DarkDiv)
dat <- read.csv("data_biomac_2.csv")
dim(dat)
#community matrix
comm <- dat[, -(1:5)]
#oppure comm <- dat[, 6:ncol(dat)] dalla 6 all'ultima colonna
dark_comm <- DarkDiv(comm) #dark-diversity, le specie non presenti
str(dark_comm)
rowSums(dark_comm$Dark) #only NA (not avaiable) se c'è un NA diventa tutto NA, need to remove it
dd <- rowSums(dark_comm$Dark, na.rm = T) #now we have results
hist(dd) # not normally distributed
# one continuos variable e two 
