##### Script calcul indice Bray-Curtis diversité bêta sur moyennes des parcelles #####
library(tidyverse)

##Garder que les données 2026
données_2026 <- prairie_sp_clean %>% 
  filter(Annee == "2026")

#Garder que les données utilisables en 2026
données_brutes <- données_2026 %>%
  select(-c(1:2))

#Faire des moyennes d'abondance pour chaque parcelle
moyennes_parcelles <- données_2026 %>% group_by(Parcelles) %>% summarise(across(where(is.numeric), mean, na.rm = TRUE))

#Garder que les données utilisables et nommer les parcelles
données_brutes_mean <- moyennes_parcelles %>%
  select(-c(1:2))

rownames(données_brutes_mean) <- c("Chemin blanc", "Gite", "Grande parcelle", "Mesnil st père", "Mesnil st père 2", "Parcelle regeneree")

# Calculer la matrice de dissimilarité de Bray-Curtis entre les moyennes des différentes parcelles
bray_dist <- vegdist(données_brutes_mean, method = "bray")

# Convertir en matrice complète
as.matrix(bray_dist) 

# Visualiser avec un dendrogramme
plot(hclust(bray_dist), main = "Dendrogramme de l'indice de Bray-Curtis entre les différentes parcelles", )


##### Script calcul indice Bray-Curtis diversité bêta sur moyennes des parcelles #####

##Garder que les données 2026
données_2026 <- prairie_sp_clean %>% 
  filter(Annee == "2026")

# Changer les noms des lignes pour avoir id_parcelle
quadrats_2026 <- données_2026 %>%
  group_by(Parcelles) %>%
  mutate(id = paste0(row_number(), "_", Parcelles))%>%
  ungroup() %>%
  column_to_rownames("id")

#Garder que les données utilisables et nommer les parcelles
donnée_brute <- quadrats_2026 %>%
  select(-c(1:2))

# Calculer la matrice de dissimilarité de Bray-Curtis entre les moyennes des différentes parcelles
bray_dist <- vegdist(donnée_brute, method = "bray")

# Convertir en matrice complète
as.matrix(bray_dist) 

# Visualiser avec un dendrogramme
tree = hclust(bray_dist)
plot(tree, main = "Dendrogramme de l'indice de Bray-Curtis entre les différentes parcelles")
rect.hclust(tree, k=4)



##### Différence de diversité Bêta sur Ménil St Pierre 2 entre 2019 et 2026 #####
##Garder que les données 2019, 2023, 2024, 2026
données_2619 <- prairie_sp_clean %>% 
  filter(Annee %in% c("2026", "2019", "2023", "2024"))

##Garder que les données Ménil st père 2
menilstpere_2 <- données_2619 %>% 
  filter(Parcelles == "Mesnil st père 2")

# Changer les noms des lignes pour avoir id_parcelle
quadrat_mst <- menilstpere_2 %>%
  group_by(Parcelles) %>%
  mutate(id = paste0(row_number(), "_", Annee))%>%
  ungroup() %>%
  column_to_rownames("id")

#Garder que les données utilisables et nommer les parcelles
donnée_brute_mst <- quadrat_mst %>%
  select(-c(1:2))

# Calculer la matrice de dissimilarité de Bray-Curtis entre les moyennes des différentes parcelles
bray_dist <- vegdist(donnée_brute_mst, method = "bray")

# Convertir en matrice complète
as.matrix(bray_dist) 

# Visualiser avec un dendrogramme
plot(hclust(bray_dist), main = "Dendrogramme de l'indice de Bray-Curtis entre 2019, 2023, 2024, 2026 à Mesnil St Pierre 2")


tree = hclust(bray_dist)
plot(tree, main = "Dendrogramme de l'indice de Bray-Curtis entre 2019, 2022, 2023, 2026 à Mesnil St Pierre 2")
rect.hclust(tree,k=4)

 



##### Différence de diversité Bêta sur Ménil St Pierre 1 entre 2019, 2024 et 2026 #####
##Garder que les données 2019, 2022, 2024, 2026
données_261924 <- prairie_sp_clean %>% 
  filter(Annee %in% c("2026", "2019", "2023", "2024"))

##Garder que les données Ménil st père 2
menilstpere_1 <- données_261924 %>% 
  filter(Parcelles == "Mesnil st père")

# Changer les noms des lignes pour avoir id_parcelle
quadrat_mst <- menilstpere_1 %>%
  group_by(Parcelles) %>%
  mutate(id = paste0(row_number(), "_", Annee))%>%
  ungroup() %>%
  column_to_rownames("id")

#Garder que les données utilisables et nommer les parcelles
donnée_brute_mst <- quadrat_mst %>%
  select(-c(1:2))

# Calculer la matrice de dissimilarité de Bray-Curtis entre les moyennes des différentes parcelles
bray_dist <- vegdist(donnée_brute_mst, method = "bray")

# Convertir en matrice complète
as.matrix(bray_dist) 

# Visualiser avec un dendrogramme
plot(hclust(bray_dist), main = "Dendrogramme de l'indice de Bray-Curtis entre entre 2019, 2023, 2024, 2026 à Mesnil St Pierre 1")


tree = hclust(bray_dist)
plot(tree, main = "Dendrogramme de l'indice de Bray-Curtis entre les différentes parcelles")


##### Différence de diversité Bêta sur Grande parcelle entre 2019, 2022, 2024 et 2026 #####
##Garder que les données 2019, 2022, 2024, 2026
données_26192422 <- prairie_sp_clean %>% 
  filter(Annee %in% c("2026", "2019", "2022", "2024"))

##Garder que les données Grande parcelle
GrParcelle <- données_26192422 %>% 
  filter(Parcelles == "Grande parcelle")

# Changer les noms des lignes pour avoir id_parcelle
quadrat_Gp <- GrParcelle %>%
  group_by(Parcelles) %>%
  mutate(id = paste0(row_number(), "_", Annee))%>%
  ungroup() %>%
  column_to_rownames("id")

#Garder que les données utilisables et nommer les parcelles
donnée_brute_Gp <- quadrat_Gp %>%
  select(-c(1:2))

# Calculer la matrice de dissimilarité de Bray-Curtis entre les moyennes des différentes parcelles
bray_dist <- vegdist(donnée_brute_Gp, method = "bray")

# Convertir en matrice complète
as.matrix(bray_dist) 

# Visualiser avec un dendrogramme
plot(hclust(bray_dist), main = "Dendrogramme de l'indice de Bray-Curtis entre entre 2019, 2022, 2024, 2026 à Grande Parcelle")


tree = hclust(bray_dist)
plot(tree, main = "Dendrogramme de l'indice de Bray-Curtis entre les différentes parcelles")



##### Différence de diversité Bêta sur Gite entre 2019, 2022, 2024 et 2026 #####
##Garder que les données 2019, 2022, 2024, 2026
données_26192422 <- prairie_sp_clean %>% 
  filter(Annee %in% c("2026", "2019", "2022", "2024"))

##Garder que les données Grande parcelle
Gite <- données_26192422 %>% 
  filter(Parcelles == "Gite")

# Changer les noms des lignes pour avoir id_parcelle
quadrat_G <- Gite %>%
  group_by(Parcelles) %>%
  mutate(id = paste0(row_number(), "_", Annee))%>%
  ungroup() %>%
  column_to_rownames("id")

#Garder que les données utilisables et nommer les parcelles
donnée_brute_G <- quadrat_G %>%
  select(-c(1:2))

# Calculer la matrice de dissimilarité de Bray-Curtis entre les moyennes des différentes parcelles
bray_dist <- vegdist(donnée_brute_G, method = "bray")

# Convertir en matrice complète
as.matrix(bray_dist) 

# Visualiser avec un dendrogramme
plot(hclust(bray_dist), main = "Dendrogramme de l'indice de Bray-Curtis entre entre 2019, 2022, 2024, 2026 à Gite")


tree = hclust(bray_dist)
plot(tree, main = "Dendrogramme de l'indice de Bray-Curtis entre les différentes parcelles")


##### Test variation moyennes des indices de Bray-Curtis entre les parselles #####

##Garder que les données gite 26
gite_26 <- données_2026 %>% 
  filter(Parcelles == "Gite")

#Garder que les données utilisables
gite_26 <- gite_26 %>%
  select(-c(1:2))

# Calculer la matrice de dissimilarité de Bray-Curtis entre les quadrats de la parcelle Gites
bray_dist_gite <- vegdist(gite_26, method = "bray")

#Calculer la matrice de dissimilarité de Jaccard
bray_dist_gite <- vegdist(gite_26, method = "jaccard")

#Calculer la matrice de dissimilarité de Sorensen 
bray_dist_gite <- vegdist(gite_26, method = "binary")

# Calculer la moyenne de l'indice de Bray-Curtis
moyenne_gite <- mean(bray_dist_gite)
print(moyenne_gite)

# Convertir en matrice complète
as.matrix(bray_dist_gite)

# Visualiser avec un dendrogramme
plot(hclust(bray_dist_gite), main = "Dendrogramme de l'indice de Bray-Curtis entre les différentes parcelles", )

##Garder que les données grande prairie 26
grprairie_26 <- données_2026 %>% 
  filter(Parcelles == "Grande parcelle")

#Garder que les données utilisables
grprairie_26 <- grprairie_26 %>%
  select(-c(1:2))

# Calculer la matrice de dissimilarité de Bray-Curtis entre les quadrats de la parcelle Gites
bray_dist_grp <- vegdist(grprairie_26, method = "bray")

#Calculer la matrice de dissimilarité de Jaccard
bray_dist_grp <- vegdist(grprairie_26>0, method = "jaccard")

# Calculer la moyenne de l'indice de Bray-Curtis
moyenne_grp <- mean(bray_dist_grp)
print(moyenne_grp)

# Convertir en matrice complète
as.matrix(bray_dist_grp)

# Visualiser avec un dendrogramme
plot(hclust(bray_dist_grp), main = "Dendrogramme de l'indice de Bray-Curtis entre les différentes parcelles", )



## Garder que les données grande prairie 26
mstprairie_26 <- données_2026 %>% 
  filter(Parcelles == "Mesnil st père 2")

# Garder que les données utilisables
mstprairie_26 <- mstprairie_26 %>%
  select(-c(1:2))

# Calculer la matrice de dissimilarité de Bray-Curtis entre les quadrats de la parcelle Gites
bray_dist_mst <- vegdist(mstprairie_26, method = "bray")

#Calculer la matrice de dissimilarité de Jaccard
bray_dist_mst <- vegdist(mstprairie_26>0, method = "jaccard")

# Calculer la moyenne de l'indice de Bray-Curtis
moyenne_mst <- mean(bray_dist_mst)
print(moyenne_mst)
