setwd("/users/Amina/Desktop/biogeo")
dat <- read.csv("data_biomac_3.csv")
library(vegan)
str(dat[, 1:10]) #nomi delle prime dieci colonne
dim(dat) #numero di row (1561 observations) e columns (873 variables - levels e species)
873 - 7 #number of species (number of columns minus the number of columns with levels) = 866
com <- dat[, -(1:7)] #creo la community togliendo le prime 7 colonne contenendo info che non son le specie
specnumber(com) #species richness Margin 1 lavora con le row
specnumber(com, MARGIN = 2) #species frequency   Margin 2 gli dice di lavorare con le colonne
rowSums(com > 0) # eliminoo???
hist(specnumber(com), xlab = "Species richness", ylab = "Number of plots", main = NULL, breaks = )
summary(specnumber(com) # min richness = 1, median = 13, mean = 15.14, max richness = 52
hist(specnumber(com, MARGIN = 2, xlab = "species frequency") #molte specie hanno una bassissima freq, lots of rare species
which.max(specnumber(com)) #species with max richness
elba <- com[dat$island == "Elba", ] subset com con la condizione che le rows devono avere elba
which.max(sort(specnumber(elba, MARGIN = 2), decreasing = T))) #la specie più frequente sull'isola d'elba. Oppure togli which.max e metti alla fine [1]
sum(specnumber(com, MARGIN = 2) == 1) # number of species occurring only once
head(sort(specnumber(com, MARGIN = 2), decreasing = T), 15) # first 15 species with higher freq. Oppure lasciavo via head e aggiungevo solo alla fine [1:15]

specnumber(com, groups = dat$island) #gamma diversity between islands
specnumber(com, groups = dat$habitat) #gamma diversity between habitats

adipart(com, dat[, c(3, 5, 6, 7), nsimul = 9) #additional diversity
multipart(com, dat[, c(3,5,6,7), nsimul = 9) # multiplicative diversity
                   


######## part 2
library(vegan)
library(Hmisc)
library(betapart9
dat <- read.csv("data_biomac_2.csv")
dim(dat)
dat[1:5, 1:10]
comm <- dat[, -(1:5)]
comm_pa <- decostand(comm, method = "pa")
adipart(comm_pa)
#adipart calcola le alpha, beta e gamma diversity, statistic is the observed value (ho 84 specie in tot e in media 11 sono presenti sono in un plot) -> additive B-diversity
adipart(comm_pa, dat[, 1:4]) #per ottenere anche alpha 2 e 3 e beta 2 e 3, oltre all'1
#beta1 is significant smaller than aspected (non ho capito perchè)
#beta3 indica che most of diversity is due to differences between regions
multipart(comm_pa, scales = 0)
#multi sta per multiplicative B-diversity. L'unico cambiato è beta xk 84/11 = 7.6 (prima era 84-11=73)
multipart(comm_pa, dat[, c(1, 5, 4)], scales = 0)
#perchè voglio usare i livelli presenti nelle colonne 1 5 e 4
#a1 * B1 = a2, a2*B2 =gamma
#level contributing the most to diversity is B1 (plot)
