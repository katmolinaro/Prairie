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


##Garder que les données grande prairie 26
gite_26 <- données_2026 %>% 
  filter(Parcelles == "Gite")

#Garder que les données utilisables
gite_26 <- gite_26 %>%
  select(-c(1:2))

# Calculer la matrice de dissimilarité de Bray-Curtis entre les quadrats de la parcelle Gites
bray_dist_gite <- vegdist(gite_26, method = "bray")

# Calculer la moyenne de l'indice de Bray-Curtis
moyenne_gite <- mean(bray_dist_gite)
print(moyenne_gite)

# Convertir en matrice complète
as.matrix(bray_dist_gite)

# Visualiser avec un dendrogramme
plot(hclust(bray_dist_gite), main = "Dendrogramme de l'indice de Bray-Curtis entre les différentes parcelles", )

##Garder que les données grande prairie 26
gite_26 <- données_2026 %>% 
  filter(Parcelles == "Gite")

#Garder que les données utilisables
gite_26 <- gite_26 %>%
  select(-c(1:2))

# Calculer la matrice de dissimilarité de Bray-Curtis entre les quadrats de la parcelle Gites
bray_dist_gite <- vegdist(gite_26, method = "bray")

# Calculer la moyenne de l'indice de Bray-Curtis
moyenne_gite <- mean(bray_dist_gite)
print(moyenne_gite)

# Convertir en matrice complète
as.matrix(bray_dist_gite)

# Visualiser avec un dendrogramme
plot(hclust(bray_dist_gite), main = "Dendrogramme de l'indice de Bray-Curtis entre les différentes parcelles", )

