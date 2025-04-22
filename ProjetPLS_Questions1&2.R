#Packages
require("ggplot2")
require("factoextra")
require("FactoMineR")
require("car")
require("GGally")
require("reshape2")
require("corrplot")
require("car")

# Importation du fichier
data <- read.table("C:/Users/HP/OneDrive/Bureau/M2 IMSD/Cours/PLS/Projet PLS/Résultats_présidentielles_2022.txt", 
                   header = TRUE)

# data <- read.table("C:/Users/HP/OneDrive/Bureau/M2 IMSD/Cours/PLS/Projet PLS/Résultats_présidentielles_2022.txt", 
#                  header = TRUE, sep = "\t", stringsAsFactors = FALSE, dec = ".", check.names = FALSE)

head(data)
colnames(data) 
dim(data)
sapply(data, class) # Type des colonnes

# Suppression des deux colonnes de  la base (Num_dept", "Nom_dept)
data2 <- data[, !(names(data) %in% c("Num_dept", "Nom_dept"))]
dim(data2)

summary(data2)

#Premier tour
dep <- data[which.min(data$Inscrits_1), ]
print(dep[c("Nom_dept", "Inscrits_1")])
dep <- data[which.max(data$Inscrits_1), ]
print(dep[c("Nom_dept", "Inscrits_1")])

dep <- data[which.min(data$Abstentions_1), ]
print(dep[c("Nom_dept", "Abstentions_1")])
dep <- data[which.max(data$Abstentions_1), ]
print(dep[c("Nom_dept", "Abstentions_1")])

dep <- data[which.min(data$Votants_1), ]
print(dep[c("Nom_dept", "Votants_1")])
dep <- data[which.max(data$Votants_1), ]
print(dep[c("Nom_dept", "Votants_1")])

dep <- data[which.min(data$Blancs_1), ]
print(dep[c("Nom_dept", "Blancs_1")])
dep <- data[which.max(data$Blancs_1), ]
print(dep[c("Nom_dept", "Blancs_1")])

dep <- data[which.min(data$Nuls_1), ]
print(dep[c("Nom_dept", "Nuls_1")])
dep <- data[which.max(data$Nuls_1), ]
print(dep[c("Nom_dept", "Nuls_1")])

dep <- data[which.min(data$Exprimes_1), ]
print(dep[c("Nom_dept", "Exprimes_1")])
dep <- data[which.max(data$Exprimes_1), ]
print(dep[c("Nom_dept", "Exprimes_1")])


#Second tour
dep <- data[which.min(data$Inscrits_2), ]
print(dep[c("Nom_dept", "Inscrits_2")])
dep <- data[which.max(data$Inscrits_2), ]
print(dep[c("Nom_dept", "Inscrits_2")])

dep <- data[which.min(data$Abstentions_2), ]
print(dep[c("Nom_dept", "Abstentions_2")])
dep <- data[which.max(data$Abstentions_2), ]
print(dep[c("Nom_dept", "Abstentions_2")])

dep <- data[which.min(data$Votants_2), ]
print(dep[c("Nom_dept", "Votants_2")])
dep <- data[which.max(data$Votants_2), ]
print(dep[c("Nom_dept", "Votants_2")])

dep <- data[which.min(data$Blancs_2), ]
print(dep[c("Nom_dept", "Blancs_2")])
dep <- data[which.max(data$Blancs_2), ]
print(dep[c("Nom_dept", "Blancs_2")])

dep <- data[which.min(data$Nuls_2), ]
print(dep[c("Nom_dept", "Nuls_2")])
dep <- data[which.max(data$Nuls_2), ]
print(dep[c("Nom_dept", "Nuls_2")])

dep <- data[which.min(data$Exprimes_2), ]
print(dep[c("Nom_dept", "Exprimes_2")])
dep <- data[which.max(data$Exprimes_2), ]
print(dep[c("Nom_dept", "Exprimes_2")])

# Cartographie avant standardisation
data2
z <- t(as.matrix(data2))
x <- seq(1,26,length.out=26)
y <- seq(1,107,length.out=107)
rownames(z) <- c("Inscrits_1", "Abstentions_1", "Votants_1", "Blancs_1",
                 "Nuls_1", "Exprimes_1", "ARTHAUD", "ROUSSEL", "MACRON", "LASSALLE",
                 "LE_PEN", "ZEMMOUR", "MELENCHON", "HIDALGO", "JADOT", "PECRESSE", "POUTOU",
                 "DUPONT_AIGNAN", "Inscrits_2", "Abstentions_2", "Votants_2", "Blancs_2",
                 "Nuls_2", "Exprimes_2", "MACRON_2", "LE_PEN_2")
image(x,y,z,xlab="Variables",ylab="Departements",main="Rrésultats des élections presidentielles",axes=FALSE)
axis(1, at = x,labels = rownames(z),las=1,cex.axis=1)
axis(2, at = y,labels = colnames(z),las=2,cex.axis=0.6)


# Cartographie apres standardisation
data2scale<-scale(data2)
z <- t(as.matrix(data2scale))
x <- seq(1,26,length.out=26)
y <- seq(1,107,length.out=107)
rownames(z) <- c("Inscrits_1", "Abstentions_1", "Votants_1", "Blancs_1",
                 "Nuls_1", "Exprimes_1", "ARTHAUD", "ROUSSEL", "MACRON", "LASSALLE",
                 "LE_PEN", "ZEMMOUR", "MELENCHON", "HIDALGO", "JADOT", "PECRESSE", "POUTOU",
                 "DUPONT_AIGNAN", "Inscrits_2", "Abstentions_2", "Votants_2", "Blancs_2",
                 "Nuls_2", "Exprimes_2", "MACRON_2", "LE_PEN_2")
image(x,y,z,xlab="Variables",ylab="Departements",main="Résultats des élections presidentielles", axes=FALSE)
axis(1, at = x,labels = rownames(z),las=1,cex.axis=1)
axis(2, at = y,labels = colnames(z),las=2,cex.axis=0.6)

##Question 1##

#Histogrammes et courbes des densités

data2scale <- as.data.frame(data2scale)

vars_to_plot <- c("Inscrits_1", "Abstentions_1", "Votants_1", "Blancs_1",
                  "Nuls_1", "Exprimes_1", "ARTHAUD", "ROUSSEL", "MACRON", "LASSALLE",
                  "LE_PEN", "ZEMMOUR", "MELENCHON", "HIDALGO", "JADOT", "PECRESSE", "POUTOU",
                  "DUPONT_AIGNAN", "Inscrits_2", "Abstentions_2", "Votants_2", "Blancs_2",
                  "Nuls_2", "Exprimes_2", "MACRON_2", "LE_PEN_2")
for (var in vars_to_plot) {
  binwidth_value <- diff(range(data2scale[[var]], na.rm = TRUE)) / 30
  
  p <- ggplot(data2scale, aes_string(x = var)) +
    geom_histogram(aes(y = ..density..), binwidth = binwidth_value, fill = "skyblue", color = "black", alpha = 0.6) +
    geom_density(color = "red", fill = "red", alpha = 0.2) +
    ggtitle(paste("Distribution de", var)) +
    theme_minimal() +
    xlab(var) +
    ylab("Densité")
  
  print(p) 
}


# Scatterplots
ggpairs(data2scale[,  c("Inscrits_1", "Abstentions_1", "Votants_1", "Blancs_1",
                        "Nuls_1", "Exprimes_1", "ARTHAUD", "ROUSSEL", "MACRON", "LASSALLE",
                        "LE_PEN", "ZEMMOUR", "MELENCHON", "HIDALGO", "JADOT", "PECRESSE", "POUTOU",
                        "DUPONT_AIGNAN", "Inscrits_2", "Abstentions_2", "Votants_2", "Blancs_2",
                        "Nuls_2", "Exprimes_2", "MACRON_2", "LE_PEN_2")])



# Matrice de corrélations donnee scalées
cor_data2scale <- cor(data2scale)
cor_data2scale
col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white",
                           "cyan", "#007FFF", "blue", "#00007F"))
col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                           "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                           "#4393C3", "#2166AC", "#053061"))
col3 <- colorRampPalette(c("red", "white", "blue")) 
col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F",
                           "cyan", "#007FFF", "blue", "#00007F"))
whiteblack <- c("white", "black")
corrplot(cor_data2scale, method = "color", addrect = 2, col = col1(100))
ggcorr(data2scale)

#############################################################################

# Proportion non scale
data2
dim(data2)
premier_tour1 <- c('Abstentions_1',"Votants_1", 'Blancs_1', 'Nuls_1', 'Exprimes_1', 
                   'ARTHAUD', 'ROUSSEL', 'MACRON', 'LASSALLE', 'LE_PEN', 
                   'ZEMMOUR', 'MELENCHON', 'HIDALGO', 'JADOT', 'PECRESSE', 
                   'POUTOU', 'DUPONT_AIGNAN')
donnees_premier_tour1 <- data2[, premier_tour1]
proportions_11 <- data2[premier_tour1] / data2$Inscrits_1
dim(proportions_11)

second_tour2 <-c('Abstentions_2',"Votants_2", 'Blancs_2', 'Nuls_2', 'MACRON_2', 'LE_PEN_2') 
donnees_second_tour2 <- data2[, second_tour2]
proportions_22 <- data2[second_tour2] / data2$Inscrits_2
dim(proportions_22)


proportions_T2 <- cbind(proportions_11, proportions_22)
proportions_T2
dim(proportions_T2)
############################################################################


### proportions scalé
data2scale
dim(data2scale)
premier_tour <- c('Abstentions_1',"Votants_1", 'Blancs_1', 'Nuls_1', 'Exprimes_1', 
                  'ARTHAUD', 'ROUSSEL', 'MACRON', 'LASSALLE', 'LE_PEN', 
                  'ZEMMOUR', 'MELENCHON', 'HIDALGO', 'JADOT', 'PECRESSE', 
                  'POUTOU', 'DUPONT_AIGNAN')
donnees_premier_tour <- data2scale[, premier_tour]
proportions_1 <- data2scale[premier_tour] / data2scale$Inscrits_1
dim(proportions_1)

second_tour <-c('Abstentions_2',"Votants_2", 'Blancs_2', 'Nuls_2',"Exprimes_1", 'MACRON_2', 'LE_PEN_2') 
donnees_second_tour <- data2scale[, second_tour]
proportions_2 <- data2scale[second_tour] / data2scale$Inscrits_2
dim(proportions_2)

proportions_T <- cbind(proportions_1, proportions_2)
proportions_T
dim(proportions_T)
#########################################################"
# Matrice de corrélations, scale apres prop
#proportions_T_Scale<-scale(proportions_T)

cor_proportions_T_Scale <- cor(proportions_T)
cor_proportions_T_Scale
col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white",
                           "cyan", "#007FFF", "blue", "#00007F"))
col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                           "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                           "#4393C3", "#2166AC", "#053061"))
col3 <- colorRampPalette(c("red", "white", "blue")) 
col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F",
                           "cyan", "#007FFF", "blue", "#00007F"))
whiteblack <- c("white", "black")
corrplot(cor_proportions_T_Scale, method = "color", addrect = 2, col = col1(100))
ggcorr(proportions_T)




####Question2

####### ACP premier tour

#donnees_premier_tour <- proportions_T_Scale[, 1:16]
#donnees_premier_tour <- sapply(donnees_premier_tour, as.numeric)
resultat_acp1 <- PCA(proportions_11, scale.unit = TRUE, 
                     ncp = min(ncol(proportions_11),
                               nrow(proportions_11)) - 1, graph = FALSE)




#resultat_acp1 <- PCA(proportions_11,scale.unit = TRUE,graph = TRUE)
#print(resultat_acp1)
#summary(resultat_acp1)
#print(resultat_acp1$eig[, "eigenvalue"])

#variance expliquée par chaque composante principale
variance_expliquee <- resultat_acp1$eig / sum(resultat_acp1$eig)
variance_expliquee

# Pourcentage des Variances expliquées des variables
fviz_eig(resultat_acp1, addlabels = TRUE, ylim = c(0, 100),)


# Contribution des variables aux axes 1 et 2  
fviz_pca_var(resultat_acp1, axes = c(1, 2), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

# Contributions des variables sur toutes les dimensions
var_contrib <- get_pca_var(resultat_acp1)$contrib
corrplot(var_contrib, is.corr=FALSE)  

# Qualité de représentation (cos2) des Variables
fviz_pca_var(resultat_acp1, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             axes = c(1, 2),
             labelsize = 3, 
             repel = TRUE) + theme_minimal() +
  labs(title = "Qualité de Représentation (cos2) des Variables")


# Corrélations entre les variables et les axes (composantes)
fviz_pca_var(resultat_acp1, col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             axes = c(1, 2), 
             labelsize = 3,
             repel = TRUE) +
  theme_minimal() +
  labs(title = "Corrélations entre les Variables et les Composantes")

# Contribution des individus
fviz_pca_ind(resultat_acp1, axes = c(1, 2), col.ind = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme_minimal() +
  ggtitle("Représentation des individus - ACP")

# Qualité de représentation (cos2) des individus
fviz_pca_ind(resultat_acp1, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             axes = c(1, 2),
             labelsize = 3, 
             repel = TRUE) + theme_minimal() +
  labs(title = "Qualité de Représentation (cos2) des ind")

# Distances entre les ind et les axes (composantes)
fviz_pca_ind(resultat_acp1, col.var = "dist", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             axes = c(1, 2), 
             labelsize = 3,
             repel = TRUE) +
  theme_minimal() +
  labs(title = "Distance entre les ind et les Composantes")

var_coords <- resultat_acp1$var$coord
var_coords

resultat_acp1$
  
  ####### ACP deuxième tour
  #donnees_deuxième_tour<- proportions_T_Scale[, 17:21]
  #donnees_deuxième_tour <- sapply(donnees_deuxième_tour, as.numeric)
  #resultat_acp2 <- PCA(proportions_22,scale.unit = TRUE,graph = TRUE)
proportions_22
proportions_22
proportions_22
proportions_22
resultat_acp2 <- PCA(proportions_22, scale.unit = TRUE, 
                     ncp = min(ncol(proportions_22),
                               nrow(proportions_22)) - 1, graph = FALSE)

print(resultat_acp2)
summary(resultat_acp2)
print(resultat_acp2$eig[, "eigenvalue"])

#variance expliquée par chaque composante principale
variance_expliquee <- resultat_acp2$eig / sum(resultat_acp2$eig)
variance_expliquee

# Pourcentage des Variances expliquées des variables
fviz_eig(resultat_acp2, addlabels = TRUE, ylim = c(0, 100),)


# Contribution des variables aux axes 1 et 2  
fviz_pca_var(resultat_acp2, axes = c(1, 2), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

# Contributions des variables sur toutes les dimensions
var_contrib <- get_pca_var(resultat_acp2)$contrib
corrplot(var_contrib, is.corr=FALSE)  

# Qualité de représentation (cos2) des Variables
fviz_pca_var(resultat_acp2, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             axes = c(1, 2),
             labelsize = 3, 
             repel = TRUE) + theme_minimal() +
  labs(title = "Qualité de Représentation (cos2) des Variables")


# Corrélations entre les variables et les axes (composantes)
fviz_pca_var(resultat_acp2, col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             axes = c(1, 2), 
             labelsize = 3,
             repel = TRUE) +
  theme_minimal() +
  labs(title = "Corrélations entre les Variables et les Composantes")

# Contribution des individus
fviz_pca_ind(resultat_acp2, axes = c(1, 2), col.ind = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme_minimal() +
  ggtitle("Représentation des individus - ACP")

# Qualité de représentation (cos2) des individus
fviz_pca_ind(resultat_acp2, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             axes = c(1, 2),
             labelsize = 3, 
             repel = TRUE) + theme_minimal() +
  labs(title = "Qualité de Représentation (cos2) des ind")

# Distances entre les ind et les axes (composantes)
fviz_pca_ind(resultat_acp2, col.var = "dist", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             axes = c(1, 2), 
             labelsize = 3,
             repel = TRUE) +
  theme_minimal() +
  labs(title = "Distance entre les ind et les Composantes")

var_coords <- resultat_acp2$var$coord
var_coords


##ACP sur les deux tours simultanement
##################################################


#donnees_les_deux_tour<- proportions_T_Scale
#donnees_deuxième_tour <- sapply(donnees_deuxième_tour, as.numeric)
#resultat_acp <- PCA(proportions_T2,scale.unit = TRUE, graph = TRUE)
#print(resultat_acp)
#summary(resultat_acp)

resultat_acp <- PCA(proportions_T2, scale.unit = TRUE, 
                    ncp = min(ncol(proportions_T2),
                              nrow(proportions_T2)) - 1, graph = FALSE)

# Les valeurs propres
print(resultat_acp$eig[, "eigenvalue"])

#variance expliquée par chaque composante principale
variance_expliquee <- resultat_acp$eig / sum(resultat_acp$eig)
variance_expliquee

# Pourcentage des Variances expliquées des variables
fviz_eig(resultat_acp, addlabels = TRUE, ylim = c(0, 100),)


# Contribution des variables aux axes 1 et 2  
fviz_pca_var(resultat_acp, axes = c(1, 2), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

# Contributions des variables sur toutes les dimensions
var_contrib <- get_pca_var(resultat_acp)$contrib
corrplot(var_contrib, is.corr=FALSE)  

# Qualité de représentation (cos2) des Variables
fviz_pca_var(resultat_acp, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             axes = c(1, 2),
             labelsize = 3, 
             repel = TRUE) + theme_minimal() +
  labs(title = "Qualité de Représentation (cos2) des Variables")


# Corrélations entre les variables et les axes (composantes)
fviz_pca_var(resultat_acp, col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             axes = c(1, 2), 
             labelsize = 3,
             repel = TRUE) +
  theme_minimal() +
  labs(title = "Corrélations entre les Variables et les Composantes")



# Contribution des individus
fviz_pca_ind(resultat_acp, axes = c(1, 2), col.ind = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme_minimal() +
  ggtitle("Représentation des individus - ACP")

# Qualité de représentation (cos2) des individus
fviz_pca_ind(resultat_acp, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             axes = c(1, 2),
             labelsize = 3, 
             repel = TRUE) + theme_minimal() +
  labs(title = "Qualité de Représentation (cos2) des ind")


# Distances entre les ind et les axes (composantes)
fviz_pca_ind(resultat_acp, col.var = "dist", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             axes = c(1, 2), 
             labelsize = 3,
             repel = TRUE) +
  theme_minimal() +
  labs(title = "Distance entre les ind et les Composantes")


#################################################

# Question 3
dim(proportions_1)
dim(proportions_2)
dim(proportions_T)

str(proportions_1)
str(proportions_2)
str(proportions_T)

colnames(proportions_T)

###################### Regressions MCO #####################################
#Abstentions
proportions_Metropole <- proportions_T[-(96:107), ]
dim(proportions_Metropole)
colnames(proportions_Metropole)

#Abstentions
#Pour calculer le VIF, enlever les variables à NaN
Abstentions <- lm(Abstentions_2 ~Abstentions_1 + Blancs_1 + Nuls_1 + 
                    ARTHAUD + ROUSSEL + MACRON + LASSALLE + LE_PEN +
                    ZEMMOUR + MELENCHON + HIDALGO + JADOT + PECRESSE + POUTOU
                  , data=proportions_Metropole)
summary(Abstentions)
vif(Abstentions)
Tolrence<-1/vif(Abstentions)
Tolrence

#Nuls
#Pour calculer le VIF, enlever les variables à NaN
Nuls <- lm(Nuls_2 ~Abstentions_1 + Blancs_1 + Nuls_1 + 
             ARTHAUD + ROUSSEL + MACRON + LASSALLE + LE_PEN +
             ZEMMOUR + MELENCHON + HIDALGO + JADOT + PECRESSE + POUTOU
           , data=proportions_Metropole)
summary(Nuls)
vif(Nuls)
Tolrence<-1/vif(Nuls)
Tolrence

#MACRON
#Pour calculer le VIF, enlever les variables à NaN
MACRON <- lm(MACRON_2 ~Abstentions_1 + Blancs_1 + Nuls_1 + 
               ARTHAUD + ROUSSEL + MACRON + LASSALLE + LE_PEN +
               ZEMMOUR + MELENCHON + HIDALGO + JADOT + PECRESSE + POUTOU
             , data=proportions_Metropole)
summary(MACRON)
vif(MACRON)
Tolrence<-1/vif(MACRON)
Tolrence

#LEPEN
#Pour calculer le VIF, enlever les variables à NaN
LEPEN <- lm(LE_PEN_2 ~Abstentions_1 + Blancs_1 + Nuls_1 + 
              ARTHAUD + ROUSSEL + MACRON + LASSALLE + LE_PEN +
              ZEMMOUR + MELENCHON + HIDALGO + JADOT + PECRESSE + POUTOU
            , data=proportions_Metropole)
summary(LEPEN)
vif(LEPEN)
Tolrence<-1/vif(LEPEN)
Tolrence



############ Cartographie entre CP(deux) et Proportion_T2

loadings <- resultat_acp1$var$coord
loadings_long <- melt(loadings, id.vars = rownames(loadings), variable.name = "Composante", value.name = "Correlation")
loadings_long$Variable <- rownames(loadings)

loadings_long <- loadings_long[, -which(names(loadings_long) == "Variable")]

# Renommer les colonnes pour clarifier leur utilisation
names(loadings_long)[names(loadings_long) == "Var1"] <- "Variable"
names(loadings_long)[names(loadings_long) == "Var2"] <- "Composante"


p <- ggplot(loadings_long, aes(x = Variable, y = Composante, fill = Correlation)) +
  geom_tile() +  # cela crée les cases de la heatmap
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_text(angle = 0, hjust = 1),
        axis.title = element_blank()) +
  labs(fill = "Correlation", title = "Heatmap of PCA Loadings")

print(p)





