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

            
###
# potential range/species frequ, tra 0 e 1
sf <- specnumber(comm, MARGIN = 2) #realised range size  o species frequency          
dark_comm$Pool[1:5, 1:5] #potential range numeri con 0.blabla, 1 significa che c'è la specie           
pr <- colSums(dark_comm$Pool)
head(pr)            
rf <- sf/pr #range filling (sf/pr)            
sort(rf, decreasing = T)[1:10] #i rf più alti, the dominant species is Fagus Sylvatica
sort(rf) [1:20]  # le 20 specie con il rf più basso          
            

            
library(sars)
data(galap)
?galap
galap
summary(galap)
mod1 <- sar_power(data = galap) #power model
summary(mod1) # formula: S== C*Ael.Z (Arrhenius)
# estimate C = 33.17 -> we expect 33 species per m2
# Adj. R-squ: 0.41 of the variability is explained by the model
plot(mod1) #small islands fit better the model
mod2 <- sar_loga(galap) # model with logarithm
# attention, cambia la scale di Estimate C, il p-val is almost 1 -> large problem of fitting
plot(mod2)            
display_sars_models() #updated version of functions            
sar_pred(mod1, area = 5) #prediction of number of species in a island with an area of 5m
mod_ens <- sar_multi(galap, obj = c("power", "loga", "koba"))
sar_pred(mod_ens, area = 5)
            
