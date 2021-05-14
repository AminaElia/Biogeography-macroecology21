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
boxplot(dd~dat$Management) # data diversity as function of the two cathegories of management
# seconda opzione: boxplot(dd[dat$Management == "C"], dd[dat$Management == "F"])
t.test(dd ~ dat$Management) #t= misura del parametro, p-value = value of the probability
#differenze nei mean values, the difference is significant? No! (guardando p-value)

wilcox.test(dd~d
boxplot(dd~dat$Management) # data diversity as function of the two cathegories of managementat$           
# boxplot(dd[dat$Management == "C"], dd[dat$Management == "F"])
t.test(dd ~ dat$Management) #p-value dice se la diff tra le mean is significant (x normal dist)
wilcox.test(dd ~ dat$Management) #un altro test che dice che non è significant (x dist che non sono xforza normal)

#community complitness = ln(observed richness/dark diversity)
library(vegan)
sr <- specnumber(comm)
cc <- log(sr/dd)
summary(cc)
hist(cc)
boxplot(cc~dat$Management) #is the management affecting community completeness?
# F is much more variable, and in median more complete than C. Not normal dist            
wilcox.test(cc~dat$Management) #association looking at p-val. 
t.test(cc~dat$Management)            
