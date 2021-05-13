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

data(BCI.env)
bci_tur <- d2$beta.jtu #estraggo solo il turnover 
bci_geo <- dist(BCI.env[, 1:2]) #voglio le due colonne UTM contenenti le coord, che sono la num 1 e num 2
#result is a triangular distances' matrix
plot(BCI.env$UTM.EW, BCI.env$UTM.NS) #scatterplot
plot(bci_geo, bci_tur)
#esce un plot con righe verticali. alla stessa geo distance con dissimilarity diverse. Sono pairs.
#positive relation but maybe not significant
mod1 <- lm(bci_tur ~ bci_geo) #model
summary(mod1) #guarda Estimate(intercept e slope), Pr e Adj R-squared(quanto è espressivo il model)
plot(bci_geo, bci_tur)
abline(a = mod1$coefficients[1], b = mod1$coefficients[2], col = "red", lwd = 3)
#grafico con linea del modello  a=intercept, b=slope

#same for nestedness
bci_nes <- d2$beta.jne #extract nestedness
plot(bci_geo, bci_nes)
mod2 <- lm(bci_nes ~bci_geo)
summary(mod2) #i residue sono molto skwned, slope is negative, intercept is very small. Rsquared is extremely low.
#il model non è ben espressivo, not significant (n.s.)
# poca differenza tra distance e nestedness
plot(bci_geo, bci_nes)
abline(a = mod2$coefficients[1], b = mod2$coefficients[2], col = "red", lwd = 3)


