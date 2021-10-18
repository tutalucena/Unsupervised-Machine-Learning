###############################################################################
#Loading packages and libraryes
###############################################################################

pacotes <- c("readr","tidyverse","cluster","dendextend","factoextra","plotly",
             "ggrepel","PerformanceAnalytics", "reshape2", "dplyr","fpc",
             "readxl","gridExtra")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


###############################################################################
#Exploratory Analyses
###############################################################################

library(readr) #pacote de leitura de dados
library(readxl) #pacote de leitura de dados de excel
library(tidyverse) #pacote para manipulacao de dados
library(cluster) #algoritmo de cluster
library(dendextend) #compara dendogramas
library(factoextra) #algoritmo de cluster e visualizacao
library(fpc) #algoritmo de cluster e visualizacao
library(gridExtra) #para a funcao grid arrange


mcd <- read.table("McDonalds.csv", sep = ";", dec = ",", header = T)


# Checking features
names(mcd)

#Cheking features and dimension
str(mcd)

#Checking table firts's rows
head(x=mcd)

#Checking the last's rows
tail(x=mcd)

#Editing some values
edit(mcd)

#Lookinf for the "Gordura Tans"
summary(mcd$Gorduras.Trans)

#Looking for the "Gordura Trans" distribution
quantile(mcd$Gorduras.Trans, seq(0,1,0.1))


###############################################################################
#Hierarchical Cluster - MCDonald
###############################################################################

#transform Burguer's names in rows
rownames(mcd) <- mcd[,1]
mcd <- mcd[,-1]

#Scale features
mcd.std <- scale(mcd)

#Calculate euclidean distance
distancia <- dist(mcd.std, method = "euclidean")

#Calculate euclidean distance for following methods: "average", "single", 
#                                                    "complete" e "ward.D"
cluster.hierarquico <- hclust(distancia, method = "average" )

# Dendrogram
plot(cluster.hierarquico, cex = 0.6, hang = -1)

# Create the graph
rect.hclust(cluster.hierarquico, k = 4)

# Analysing Elbow
fviz_nbclust(mcd.std, FUN = hcut, method = "wss")


# Creating 4 clusters
grupo_lanches4 <- cutree(cluster.hierarquico, k = 4)
table(grupo_lanches4)

# Transform the the view in a data frame
Lanches_grupos <- data.frame(grupo_lanches4)

# Merging the two bases
Base_lanches_fim <- cbind(mcd, Lanches_grupos)

###############################################################################
#Descriptive Analyses
###############################################################################

# Average per group
mediagrupo <- Base_lanches_fim %>% 
  group_by(grupo_lanches4) %>% 
  summarise(n = n(),
            Valor.Energetico = mean(Valor.Energetico), 
            Carboidratos = mean(Carboidratos), 
            Proteinas = mean(Proteinas),
            Gorduras.Totais = mean(Gorduras.Totais), 
            Gorduras.Saturadas = mean(Gorduras.Saturadas), 
            Gorduras.Trans = mean(Gorduras.Trans),
            Colesterol = mean(Colesterol), 
            Fibra.Alimentar = mean(Fibra.Alimentar), 
            Sodio = mean(Sodio),
            Calcio = mean(Calcio), 
            Ferro = mean(Ferro) )

table(mediagrupo)
 
summary(mediagrupo$Valor.Energetico)
summary(mediagrupo$Carboidratos)
summary(mediagrupo$Proteinas)            
summary(mediagrupo$Gorduras.Totais)
summary(mediagrupo$Gorduras.Saturadas)
summary(mediagrupo$Gorduras.Trans)
summary(mediagrupo$Colesterol)
summary(mediagrupo$Fibra.Alimentar)
summary(mediagrupo$Sodio)
summary(mediagrupo$Ferro)
summary(mediagrupo$Calcio)

###############################################################################
#Non Hierarquical Cluster - Mcdonald
###############################################################################

# Load the model
mcd.k4 <- kmeans(mcd.std, centers = 4)

# views the clusters
fviz_cluster(mcd.k4, data = mcd.std, main = "Cluster K4")

# Creating the clusters
mcdonalds.k3 <- kmeans(mcdonalds.padronizado, centers = 3)
mcdonalds.k4 <- kmeans(mcd.std, centers = 4)
mcdonalds.k5 <- kmeans(mcdonalds.padronizado, centers = 5)

# Creating the graphs
G1 <- fviz_cluster(mcdonalds.k2, geom = "point", data = mcdonalds.padronizado) + ggtitle("k = 2")
G2 <- fviz_cluster(mcdonalds.k3, geom = "point",  data = mcdonalds.padronizado) + ggtitle("k = 3")
G3 <- fviz_cluster(mcdonalds.k4, geom = "point",  data = mcd.std) + ggtitle("k = 4")
G4 <- fviz_cluster(mcdonalds.k5, geom = "point",  data = mcdonalds.padronizado) + ggtitle("k = 5")

# Print all graphs together
grid.arrange(G3, nrow = 2)

# Veryfing Elbow
fviz_nbclust(mcd.std, kmeans, method = "wss")

plot(G3, cex = 0.6, hang = -1)
