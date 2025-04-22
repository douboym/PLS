library(plsdepot)
library(reshape2)

setwd("C:/Users/HP/OneDrive/Bureau/M2 IMSD/Cours/PLS/Projet PLS")
data <- read.table("Résultats_présidentielles_2022.txt", header = TRUE)
data<-data[1:95,]

# Calcul des proportions

pr_tour <- c('Abstentions_1',"Votants_1","Exprimes_1", 'Blancs_1', 'Nuls_1', 'ARTHAUD', 'ROUSSEL', 'MACRON', 'LASSALLE', 'LE_PEN', 'ZEMMOUR', 'MELENCHON', 'HIDALGO', 'JADOT', 'PECRESSE', 'POUTOU',"DUPONT_AIGNAN")
prop_1 <- data[pr_tour] / data$Inscrits_1

deu_tour <-c('Abstentions_2', 'Blancs_2', 'MACRON_2', 'LE_PEN_2') 
prop_2 <- data[deu_tour] / data$Inscrits_2



premier_tour <- c('Abstentions_1', 'Blancs_1', 'Nuls_1', 'ARTHAUD', 'ROUSSEL', 'MACRON', 'LASSALLE', 'LE_PEN', 'ZEMMOUR', 'MELENCHON', 'HIDALGO', 'JADOT', 'PECRESSE', 'POUTOU')
proportions_1 <- data[premier_tour] / data$Inscrits_1

deuxieme_tour <-c('Abstentions_2', 'Blancs_2', 'MACRON_2', 'LE_PEN_2') 
proportions_2 <- data[deuxieme_tour] / data$Inscrits_2

dt<- as.data.frame(cbind(prop_1, prop_2))

dt1 <- as.data.frame(cbind(proportions_1, proportions_2))

dt2<-as.data.frame(scale(dt1))






## Question 3

#### Correlation 



# Calcul de la matrice de corrélation entre les variables premier_tour et target
target<-deuxieme_tour
correlation_matrix <- cor(dt2[, c(premier_tour, target)])

# Extraction des corrélations entre premier_tour et target
correlation_df <- as.data.frame(correlation_matrix[ premier_tour, target])

colnames(correlation_df)<-paste0("cor_",colnames(correlation_df))

correlation_df<-as.data.frame(correlation_df)

# Affichage du dataframe des corrélations
print(correlation_df)







#### ols


lm_abs<- lm( Abstentions_2 ~ ., data = dt)
summary(lm_abs)




# Initialisation de la dataframe pour stocker les coefficients
teta_ols <- data.frame(matrix(NA, nrow = length(premier_tour)+1, ncol = length(target)))
colnames(teta_ols) <-  paste0("OLS_",target)
rownames(teta_ols) <- c('intercept',premier_tour)

# Création et évaluation des modèles pour chaque variable cible
for (col in paste0("OLS_",target)) {
  #print(paste("Modèle pour la variable cible :", col))
  
  # Sélection des données pour le modèle
  dt1prim<-dt1
  colnames(dt1prim)<-paste0("OLS_",colnames(dt1))
  X <- dt1[, premier_tour]
  Y <- dt1prim[, col]
  
  # Régression linéaire ordinaire (OLS)
  ols_model <- lm(Y ~ ., data = X)
  
  # Stockage des coefficients dans la dataframe teta
  teta_ols[, col] <- coef(ols_model)
  
  
}

print(teta_ols)




target<-deuxieme_tour
# Initialisation de la dataframe pour stocker les p-values
p_values_ols <- data.frame(matrix(NA, nrow = length(premier_tour)+1, ncol = length(target)))
colnames(p_values_ols) <- paste0("OLS_", target)
rownames(p_values_ols) <- c('intercept', premier_tour)

# Création et évaluation des modèles pour chaque variable cible
for (col in paste0("OLS_", target)) {
  #print(paste("Modèle pour la variable cible :", col))
  
  # Sélection des données pour le modèle
  dt1prim <- dt1
  colnames(dt1prim) <- paste0("OLS_", colnames(dt1))
  X <- dt1[, premier_tour]
  Y <- dt1prim[, col]
  
  # Régression linéaire ordinaire (OLS)
  ols_model <- lm(Y ~ ., data = X)
  
  # Stockage des p-values dans la dataframe p_values_ols
  p_values_ols[, col] <- summary(ols_model)$coefficients[, "Pr(>|t|)"]
  
  # Affichage des résultats
  #print(summary(ols_model))
  
  # Représentation des variables sur le plan (1,2)
  #plot(ols_model$fitted.values, residuals(ols_model), xlab = "Valeurs prédites", ylab = "Résidus", main = paste("Modèle pour la variable cible :", col))
  #abline(h = 0, col = "red")
  #abline(lm(residuals(ols_model) ~ ols_model$fitted.values), col = "blue")
}

print(p_values_ols)






# Initialisation de la dataframe pour stocker les R2
R_squared <- data.frame(matrix(NA, nrow = 1, ncol = length(target)))
colnames(R_squared) <- paste0("R_squared_", target)

# Création et évaluation des modèles pour chaque variable cible
for (col in paste0( target)) {
  #print(paste("Modèle pour la variable cible :", col))
  
  # Sélection des données pour le modèle
  dt1prim <- dt1
  colnames(dt1prim) <- paste0("OLS_", colnames(dt1))
  X <- dt1[, premier_tour]
  Y <- dt1[, col]
  
  # Régression linéaire ordinaire (OLS)
  ols_model <- lm(Y ~ ., data = X)
  
  # Stockage du coefficient de détermination R2 dans la dataframe R_squared
  R_squared[, paste0("R_squared_", col)] <- summary(ols_model)$r.squared
}

print(R_squared)








#### PCR

##### Calcul des composantes principales

X_sel= dt1[,c(premier_tour)]

# Calcul des composantes principales
r <- cor(X_sel)
res.eigen <- eigen(r)
v_kj <- res.eigen$vectors
z <- as.matrix(scale(X_sel))%*%v_kj
colnames(z) <- c('Z1','Z2','Z3','Z4','Z5','Z6','Z7','Z8','Z9','Z10','Z11','Z12','Z13','Z14')
dt2_z <- data.frame(dt2,z)




p <- 14
cp_max <- matrix(0,nrow=p,ncol=2)
cor_x_z <- cor(dt2_z)[1:p,18:ncol(dt2_z)]
for (j in 1:10)
{for (k in 1:10)
{if (abs(cor_x_z[j,k]) == max(abs(cor_x_z[j,]))) {cp_max[j,1]=k
cp_max[j,2]=cor_x_z[j,k]}
}
}
colnames(cp_max) <- c('Num CP','Cor max')
rownames(cp_max) <- rownames(cor_x_z)
round(cp_max,4)



# Création du dataframe pour stocker les gammes relatifs
gammes_df <- data.frame(matrix(NA, nrow = 14, ncol = length(deuxieme_tour)))
colnames(gammes_df) <- deuxieme_tour

# Boucle sur chaque cible dans deuxieme_tour
for (target in deuxieme_tour) {
  # Régression linéaire
  abs_pcr <- lm(dt2_z[, target] ~ Z1 + Z2 + Z3 + Z4, data = dt2_z)
  
  # Résumé de la régression
  abs_sum <- summary(abs_pcr)
  
  # Extraction des coefficients alpha
  alpha <- abs_sum$coefficients[2:5, 1]
  
  # Calcul des gammes
  gamma <- v_kj[, 1:4] %*% alpha
  
  # Nom des lignes et colonnes
  rownames(gamma) <- names(X_sel)
  colnames(gamma) <- paste0('PCR1_', target)
  
  # Stockage des gammes dans le dataframe
  gammes_df[, target] <- round(gamma,4)
}

# Arrondi des valeurs à quatre décimales


# Affichage du dataframe contenant les gammes
gammes_df<-as.data.frame(gammes_df)
rownames(gammes_df)<- premier_tour

print(gammes_df)



premier_tour <- c(premier_tour)

# Création et évaluation des modèles pour chaque variable cible
target <- c("Abstentions_2", "Blancs_2", "MACRON_2", "LE_PEN_2")

# Initialisation de la dataframe pour stocker les coefficients
teta_pls1 <- data.frame(matrix(NA, nrow = length(premier_tour), ncol = length(target)))
colnames(teta_pls1) <- paste0("PLS1_",target)
rownames(teta_pls1) <- premier_tour

for (col in paste0("PLS1_",target)) {
  print(paste("Modèle pour la variable cible :", col))
  
  # Sélection des données pour le modèle
  dt1prim<-dt1
  colnames(dt1prim)<-paste0("PLS1_",colnames(dt1))
  X <- dt1[, premier_tour]
  Y <- dt1prim[, col]
  
  # Régression PLS1
  pls_model <- plsreg1(X, Y, crosval =  TRUE)
  
  # Affichage des résultats
  print(pls_model)
  print(paste("R2 pour", col, ":", pls_model$R2))
  print(paste("R2Xy pour", col, ":", pls_model$R2Xy))
  print(paste("Coefficients de régression pour", col, ":", pls_model$reg.coefs))
  
  # Stockage des coefficients dans la dataframe teta
  teta_pls1[, col] <- pls_model$std.coefs
  
  # Représentation des variables sur le plan (1,2)
  plot(pls_model$cor.xyt[,1], pls_model$cor.xyt[,2], type = "n", xlab = "t1", ylab = "t2", 
       main = paste("Premier plan des variables pour", col))
  text(pls_model$cor.xyt[,1], pls_model$cor.xyt[,2], rownames(pls_model$cor.xyt))
  abline(h = 0)
  abline(v = 0)
  
  # Représentation des poids des variables sur le plan (1,2)
  plot(pls_model$raw.wgs[,1], pls_model$raw.wgs[,2],type = "n",xlab="w1",ylab="w2",main= paste0(col ," Premier plan des poids variables (t1,t2)"))
  text(pls_model$raw.wgs[,1], pls_model$raw.wgs[,2], rownames(pls_model$raw.wgs))
  abline(h=0)
  abline(v=0)
  
  # Représentation de la carte des individus sur le plan (1,2)
  plot(pls_model$x.scores[,1], pls_model$x.scores[,2],type = "n",xlab="t1",ylab="t2",main=paste0(col ," Premier plan des individus (t1,t2)"))
  text(pls_model$x.scores[,1], pls_model$x.scores[,2], rownames(X))
  abline(h=0)
  abline(v=0)
  
  # Calcul des VIP
  p <- ncol(X)
  vip <- rep(0, p)
  for (j in 1:p) {
    num <- 0
    den <- 0
    for (h in 1:2) {
      num <- num + pls_model$cor.xyt[p+1,h]^2*pls_model$raw.wgs[j,h]^2
      den <- den + pls_model$cor.xyt[p+1,h]^2
    }
    vip[j] <- sqrt(p*num/den)
  }
  
  
  # Représentation des VIP
  plot(1:p, vip, type = "n", xlab = "", ylab = "VIP", main = "Représentation des VIP")
  text(1:p, vip, rownames(X))
  abline(h = 1)
}

# Affichage des coefficients de régression pour chaque variable cible
print(teta_pls1)




# Renommer les colonnes avec les noms des variables
resul_Abs <- data.frame(
  OLS_Abstentions_2 = teta_ols$OLS_Abstentions_2[2:15],
  gammes_df_Abstentions_2 = gammes_df$Abstentions_2,  
  PLS1_Abstentions_2 = teta_pls1$PLS1_Abstentions_2,
  
  cor_Abstentions_2 = correlation_df$cor_Abstentions_2
)

# Affichage de la nouvelle dataframe avec les colonnes renommées
print(resul_Abs)




# Renommer les colonnes avec les noms des variables
resul_Blc <- data.frame(
  OLS_Blancs_2 = teta_ols$OLS_Blancs_2[2:15],
  gammes_df_Blancs_2 = gammes_df$Blancs_2,
  PLS1_Blancs_2 = teta_pls1$PLS1_Blancs_2,
  
  cor_Blancs_2 = correlation_df$cor_Blancs_2
)

# Affichage de la nouvelle dataframe avec les colonnes renommées
print(resul_Blc)



# Renommer les colonnes avec les noms des variables
resul_Macron <- data.frame(
  OLS_MACRON_2 = teta_ols$OLS_MACRON_2[2:15],
  gammes_df_MACRON_2 = gammes_df$MACRON_2,
  PLS1_MACRON_2 = teta_pls1$PLS1_MACRON_2,
  
  cor_MACRON_2 = correlation_df$cor_MACRON_2
)

# Affichage de la nouvelle dataframe avec les colonnes renommées
print(resul_Macron)


resul_Pen <- data.frame(
  OLS_LE_PEN_2 = teta_ols$OLS_LE_PEN_2[2:15],
  gammes_df_LE_PEN_2 = gammes_df$LE_PEN_2,
  PLS1_LE_PEN_2 = teta_pls1$PLS1_LE_PEN_2,
  
  cor_LE_PEN_2 = correlation_df$cor_LE_PEN_2
)
print(resul_Pen)




## Question 4

#### PLS2


num_ind<-95
n<-95

# Sélection des variables explicatives et dépendantes
X <- dt1[, premier_tour]
Y <- dt1[, deuxieme_tour]

# Régression PLS2 avec 4 composantes principales
pls_model <- plsreg2(X, Y, comps = 4)

# Affichage du modèle
print(pls_model)

# Représentation de la carte des variables sur le plan (1,2)
carte_var <- rbind(pls_model$cor.xt, pls_model$cor.yt)
plot(carte_var[,1], carte_var[,2], type = "n", xlab = "t1", ylab = "t2", main = "Premier plan des variables (t1,t2)")
text(carte_var[1:14,1], carte_var[1:14,2], rownames(carte_var), col='blue')
abline(h = 0)
abline(v = 0)


# Coefficients standardisés des prédicteurs
teta <- data.frame(pls_model$std.coefs)
colnames(teta) <- c(target)

round(teta, 4)


# Comparaison des corrélations linéaires simples et des coefficients teta standardisés

for (col in target ){
  
  
  cor_fin <- cor(dt1)[1:14,c(col)]
  nom_var <- names(cor_fin)
  cor_fin <- data.frame(cor_fin)
  teta_cor_fin <- data.frame(cor_fin, teta[,c(col)])
  t_cor <- qt(0.975, n - 1)
  a <- t_cor^2 / (n - 1)
  binf_r <- -sqrt(a / (1 + a))
  bsup_r <- sqrt(a / (1 + a))
  plot(teta_cor_fin$cor_fin, teta_cor_fin$Teta, type = "n", xlab = "Corrélation linéaire simple", ylab =  "Coefficients teta standardisés", main = col)
  text(teta_cor_fin$cor_fin, teta_cor_fin$Teta, nom_var, col = 'blue', cex = 1.3)
  abline(v = c(binf_r, 0, bsup_r))
  abline(h = 0)
}




for (col in target) {
  cor_fin <- cor(dt1[, premier_tour], dt1[, col])
  nom_var <- names(cor_fin)
  cor_fin <- data.frame(cor_fin)
  teta_cor_fin <- data.frame(cor_fin, teta[, col])
  colnames(teta_cor_fin) <- c("Correlation", col)
  t_cor <- qt(0.975, n - 1)
  a <- t_cor^2 / (n - 1)
  binf_r <- -sqrt(a / (1 + a))
  bsup_r <- sqrt(a / (1 + a))
  plot(teta_cor_fin$Correlation, teta_cor_fin[, col], type = "n", xlab = "Corrélation linéaire simple", ylab = "Coefficients teta standardisés", main = "")
  text(teta_cor_fin$Correlation, teta_cor_fin[, col], nom_var, col = 'blue', cex = 1.3)
  abline(v = c(binf_r, 0, bsup_r))
  abline(h = 0)
}





num_ind <- 95
n <- 95

# Sélection des variables explicatives et dépendantes
X <- dt1[, premier_tour]
Y <- dt1[, deuxieme_tour]

# Régression PLS2 avec 4 composantes principales
pls_model <- plsreg2(X, Y, comps = 4)

# Affichage du modèle
print(pls_model)

# Représentation de la carte des variables sur le plan (1,2)
carte_var <- rbind(pls_model$x.scores, pls_model$x.scores)
plot(carte_var[,1], carte_var[,2], type = "n", xlab = "t1", ylab = "t2", main = "Premier plan des variables (t1,t2)")
text(carte_var[1:14,1], carte_var[1:14,2], rownames(carte_var), col='blue')
abline(h = 0)
abline(v = 0)

# Représentation de la carte des individus sur le plan (1,2)
plot(pls_model$x.scores[,1], pls_model$x.scores[,2],type = "n",xlab="t1",ylab="t2",main=" Premier plan des individus (t1,t2)")
text(pls_model$x.scores[,1], pls_model$x.scores[,2], rownames(X))
abline(h=0)
abline(v=0)



# Création d'une nouvelle fenêtre graphique

# Boucle sur chaque colonne de la target
for (col in target) {
  # Calcul de la corrélation entre les variables explicatives et la variable cible
  cor_fin <- cor(dt1[, premier_tour], dt1[, col])
  
  # Obtention des noms des variables explicatives
  nom_var <- names(cor_fin)
  
  # Création d'un dataframe contenant les corrélations et les coefficients teta
  teta_cor_fin <- data.frame(Correlation = cor_fin, Teta = teta[, col])
  
  # Plot
  plot(teta_cor_fin$Correlation, teta_cor_fin$Teta, 
       xlab = "Corrélation linéaire simple", ylab = "Coefficients teta standardisés", 
       main = col, xlim = c(-1, 1), ylim = c(-1, 1))
  
  # Ajout des noms des variables explicatives avec une taille de police réduite
  text(teta_cor_fin$Correlation, teta_cor_fin$Teta, rownames(teta_cor_fin), col = 'blue', cex = 0.7)
  
  # Ajout d'une ligne horizontale à y = 0 et verticale à x = 0
  abline(h = 0, v = 0)
  
  # Ajout d'un titre pour chaque graphique
  title(main = col)
}






#### PCR2


# Assumant que dt1 est votre dataframe et que premier_tour et deuxieme_tour sont définis
num_ind <- 95
n <- 95

# Sélection des variables explicatives et dépendantes
X <- dt1[, premier_tour]
Y <- dt1[, deuxieme_tour]

# Calcul des composantes principales pour les variables explicatives
pca_model <- prcomp(X, scale. = TRUE) # scale. = TRUE pour standardiser les variables

# Sélection des 4 premières composantes principales
X_pca <- pca_model$x[, 1:4]

# Régression des variables dépendantes sur les composantes principales
pcr_model <- list()
for (i in seq_along(deuxieme_tour)) {
  dep_var <- deuxieme_tour[i]
  pcr_model[[dep_var]] <- lm(Y[, dep_var] ~ X_pca)
}

# Affichage du premier modèle PCR pour démonstration
print(summary(pcr_model[[deuxieme_tour[1]]]))

# Pour représenter les variables sur le plan des deux premières composantes principales
# Nous utilisons le loadings de pca_model
loadings <- pca_model$rotation[, 1:2]
plot(loadings[, 1], loadings[, 2], type = "n", xlab = "PC1", ylab = "PC2", main = "Premier plan des variables (PC1,PC2)")
text(loadings[, 1], loadings[, 2], rownames(loadings), col = 'blue')

# Si vous souhaitez obtenir les coefficients des composantes pour les modèles PCR:
coefficients_pcr <- sapply(pcr_model, function(model) coef(model))
print(coefficients_pcr)

