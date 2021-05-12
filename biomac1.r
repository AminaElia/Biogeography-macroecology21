#biogeo1
setwd("/users/Amina/Desktop/biogeo")
dat <- read.csv("data_biomac_1.csv")
head(dat[, 1:5]) #to see only the first 6 rows (head) and 5 columns (1:5) Indexing
dat[1:2, 1:3] #to see first 2 rows and 3 columns
dim(dat) #rows' number and columns'number (dim=dimension of the matrix)
tail(dat[, 1:5]) #last 5 columns
str(dat) #
comm <- dat[, -1] #community = dataframe except first column
#richness = contare tutti i valori maggiori di zero
comm[1:5,1:5]
comm[1:5,1:5] > 0 #false=0, true=number grater than 0
rowSums(comm[1:5,1:5] > 0) #risultato= prima linea numera la specie, la seconda conta i TRUE
rowSums(comm>0)
#species richness for the first 20 species
comm[, 1:20]
rowSums(comm[, 1:20] > 0)
#species richness for first 10 observations
rowSums(comm[1:10 ,] > 0)
specnumber(comm)#species number
comm[which(comm[,1] > 8), ] #le specie presenti nel first plot (?)
hist(specnumber(comm)) #histogramm: the most frequent richness is between 5 and 10
hist(specnumber(comm), xlab="Species richness", main=NULL)
colSums(comm > 0) #frequency(?)
?specnumber #to ask info about the function
specnumber(comm, MARGIN = 2) 
hist(specnumber(comm, MARGIN = 2)) #few species with high freq. most of the species are rare!
head(sort(specnumber(comm, MARGIN = 2), decreasing=T)) #the 6 most frequent species in the dataset
sum(specnumber(comm, MARGIN = 2) == 1) #number of species occurning only once
which(specnumber(comm, MARGIN = 2) == 1) #list of the species occuring only once
comm_pa <- decostand(comm, method ="pa") #pa sta per presence/absence, pass from aboundance to p/a
comm_pa[1:5, 1:6]
groups <- c(rep(1,10), rep(2, 9)) #repete 1 ten times e 2 nine times
specnumber(comm, groups = groups) #divido il dataset in due gruppi (non ho capito con quale criterio)
group <- rep(1, 19)
specnumber(comm, groups = group) # gamma diversity

