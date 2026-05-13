
###
#2026

# filtre année
prairie_sp_26 <- prairie_sp_clean %>% filter(Annee %in% c("2026"))
# filtre parcelles
prairie_sp_ACP26 <- prairie_sp_26 %>% filter(Parcelles %in% c("Grande parcelle","Chemin blanc", "Gite", "Parcelle regeneree", "Mesnil st père", "Mesnil st père 2"))
prairie_sp_ACPgite <- prairie_sp_26 %>% filter(Parcelles %in% c("Gite"))
# filtre colonnes vides
prairie_sp_ACP26 <- prairie_sp_ACP26[, colSums(prairie_sp_ACP26 != 0) > 0]

# Réaliser l'ACP
res.pca26 <- ade4::dudi.pca(prairie_sp_ACP26[,-(1:2)], nf = 5, scannf = F)
summary(res.pca)
#Graphes parcelles
ACP26 <- fviz_pca_ind(res.pca26, habillage = prairie_sp_26$Parcelles, addEllipses = T, geom = "point", axes = 2:3)
ACP26
#Test stats diff. parcelles
randtest(bca(res.pca26,fac = as.factor(prairie_sp_ACP26$Parcelles), nf = 6, scannf = F))



fviz_contrib(res.pca26, choice = "var", axes = 2)





































### 
#2024

# filtre année
prairie_sp_24 <- prairie_sp_clean %>% filter(Annee %in% c("2024"))
# filtre parcelles
prairie_sp_ACP24 <- prairie_sp_24 %>% filter(Parcelles %in% c("Grande parcelle","Chemin blanc", "Gite", "Parcelle regeneree", "Mesnil st père", "Mesnil st père 2"))
# filtre colonnes vides
prairie_sp_ACP24 <- prairie_sp_ACP24[, colSums(prairie_sp_ACP24 != 0) > 0]

# Réaliser l'ACP
res.pca24 <- ade4::dudi.pca(prairie_sp_ACP24[,-(1:2)], nf = 5, scannf = F)
summary(res.pca)
#Graphes parcelles
ACP24 <- fviz_pca_ind(res.pca24, habillage = prairie_sp_ACP24$Parcelles, addEllipses = TRUE, geom = "point")
ACP24
#Test stats diff. parcelles
randtest(bca(res.pca24,fac = as.factor(prairie_sp_ACP24$Parcelles), nf = 6, scannf = F))


### 
#2023

# filtre année
prairie_sp_23 <- prairie_sp_clean %>% filter(Annee %in% c("2023"))
# filtre parcelles
prairie_sp_ACP23 <- prairie_sp_23 %>% filter(Parcelles %in% c("Grande parcelle","Chemin blanc", "Gite", "Parcelle regeneree", "Mesnil st père", "Mesnil st père 2"))
# filtre colonnes vides
prairie_sp_ACP23 <- prairie_sp_ACP23[, colSums(prairie_sp_ACP23 != 0) > 0]

# Réaliser l'ACP
res.pca23 <- ade4::dudi.pca(prairie_sp_ACP23[,-(1:2)], nf = 5, scannf = F)
summary(res.pca)
#Graphes parcelles
ACP23 <- fviz_pca_ind(res.pca23, habillage = prairie_sp_ACP23$Parcelles, addEllipses = TRUE, geom = "point")
ACP23
#Test stats diff. parcelles
randtest(bca(res.pca23,fac = as.factor(prairie_sp_ACP23$Parcelles), nf = 4, scannf = F))


### 
#2022

# filtre année
prairie_sp_22 <- prairie_sp_clean %>% filter(Annee %in% c("2022"))
# filtre parcelles
prairie_sp_ACP22 <- prairie_sp_22 %>% filter(Parcelles %in% c("Grande parcelle","Chemin blanc", "Gite", "Parcelle regeneree", "Mesnil st père", "Mesnil st père 2"))
# filtre colonnes vides
prairie_sp_ACP22 <- prairie_sp_ACP22[, colSums(prairie_sp_ACP22 != 0) > 0]

# Réaliser l'ACP
res.pca22 <- ade4::dudi.pca(prairie_sp_ACP22[,-(1:2)], nf = 5, scannf = F)
summary(res.pca)
#Graphes parcelles
ACP22 <- fviz_pca_ind(res.pca22, habillage = prairie_sp_ACP22$Parcelles, addEllipses = TRUE, geom = "point")
ACP22
#Test stats diff. parcelles
randtest(bca(res.pca22,fac = as.factor(prairie_sp_ACP22$Parcelles), nf = 4, scannf = F))











# Liste pour stocker les résultats des ACP
list_graphes <- list()  # Pour les graphes ACP
list_pvalues <- list()  # Pour les p-values

# Boucle sur les années
for (annee in 2007:2026) {
    # Filtrer par année
    prairie_sp_annee <- prairie_sp_clean %>% filter(Annee == as.character(annee))
    
    # Filtrer par parcelles
    prairie_sp_ACP <- prairie_sp_annee %>% filter(Parcelles %in% c("Grande parcelle","Chemin blanc", "Gite", "Parcelle regeneree", "Mesnil st père", "Mesnil st père 2"))
    
    # Supprimer les colonnes vides
    prairie_sp_ACP <- prairie_sp_ACP[, colSums(prairie_sp_ACP != 0) > 0]
    
    # Vérifier qu'il reste des données
    if (nrow(prairie_sp_ACP) > 0 && ncol(prairie_sp_ACP) > 2) {
      # Réaliser l'ACP
      res.pca <- ade4::dudi.pca(prairie_sp_ACP[, -(1:2)], nf = 5, scannf = FALSE)
      
      # --- Test statistique ---
      # Vérifier qu'il y a au moins 2 groupes pour le test
      if (length(unique(prairie_sp_ACP$Parcelles)) >= 2) {
        test_result <- randtest(
          bca(res.pca, fac = as.factor(prairie_sp_ACP$Parcelles), nf = 4, scannf = FALSE)
        )
        p_value <- test_result$p.value
        list_pvalues[[paste0("PValue_", annee)]] <- p_value
        cat("Année", annee, "| p-value :", p_value, "\n")
      } else {
        cat("Année", annee, "| Pas assez de groupes pour le test\n")
        list_pvalues[[paste0("PValue_", annee)]] <- NA
      }
      
      # --- Graphe ACP ---
      graphe_annee <- fviz_pca_ind(
        res.pca,
        habillage = prairie_sp_ACP$Parcelles,
        addEllipses = TRUE,
        geom = "point",
        title = paste("ACP", annee)
      )
      
      # Stocker le graphe
      list_graphes[[paste0("ACP_", annee)]] <- graphe_annee
      
    } else {
      cat("Pas assez de données pour l'année", annee, "\n")
      list_pvalues[[paste0("PValue_", annee)]] <- NA
    }
}


# Combiner les graphes avec patchwork
graphe_final <- do.call(`+`, list_graphes)

# Afficher les graphes
print(graphe_final)


# Extraire les p-values et les stocker dans un data frame
df_tests <- data.frame(
  Annee = character(),
  PValue = numeric(),
  stringsAsFactors = FALSE
)

for (annee in 2007:2026) {
  test_key <- paste0("Test_", annee)
  if (test_key %in% names(list_tests)) {
    test_result <- list_tests[[test_key]]
    df_tests <- rbind(
      df_tests,
      data.frame(
        Annee = annee,
        PValue = test_result$p.value
      )
    )
  }
}

# Afficher le tableau des p-values
print(df_tests)
















fviz_pca_biplot(
  res.pca,
  addEllipses = FALSE,
  mean.point = FALSE,
  col.var = "black",
  label = "var",
  pointsize = 1,
  labelsize = 6,
  repel = TRUE
)


fviz_pca_biplot(
  res.pca,
  habillage = prairie_sp_clean$Parcelles,
  addEllipses = FALSE,
  mean.point = FALSE,
  col.var = "black",
  label = "var",
  pointsize = 2,
  labelsize = 6,
  repel = TRUE
)




# Graphes ellipses parcelles.
prairie_sp_ACP$Parcelles <- as.factor(prairie_sp_ACP$Parcelles)
parcelles <- levels(prairie_sp_ACP$Parcelles)

prairie_sp_ACP$Dim1 <- res.pca$ind$coord[, 1]
prairie_sp_ACP$Dim2 <- res.pca$ind$coord[, 2]
prairie_sp_ACP$Dim3 <- res.pca$ind$coord[, 3]

var.data <- data.frame(
  x = 0,
  y = 0,
  xend = res.pca$var$coord[, 1]*5,
  yend = res.pca$var$coord[, 2]*5,
  label = rownames(res.pca$var$coord)
)

graphiques <- list()

for (parcelle in parcelles) {
  # Créer le graphique de base avec toutes les ellipses en transparence
  p <- ggplot(prairie_sp_ACP, aes(x = Dim1, y = Dim2, color = Parcelles)) +
    # Ajouter toutes les ellipses en transparence
    stat_ellipse(level = 0.95, type = "t", linetype = "solid", alpha = 0.2) +
    # Mettre en évidence les points de l'espèce actuelle
    geom_point(data = prairie_sp_ACP[prairie_sp_ACP$Parcelles == parcelle, ], aes(color = Parcelles), size = 3) +
    # Mettre en évidence l'ellipse de l'espèce actuelle
    stat_ellipse(data = prairie_sp_ACP[prairie_sp_ACP$Parcelles == parcelle, ], level = 0.95, type = "t", linetype = "solid", color = "red", size = 1) +
    # Ajouter les flèches des variables en noir
    geom_segment(data = var.data, aes(x = x, y = y, xend = xend, yend = yend), color = "black", arrow = arrow(length = unit(0.2, "cm")), linewidth = 0.7) +
    # Ajouter les labels des variables
    geom_text(data = var.data, aes(x = xend, y = yend, label = label), color = "black", size = 4, hjust = 0.5, vjust = 0.5) +
    # Personnaliser le thème et les labels
    theme_minimal() +
    labs(
      x = paste("Dim1 (", round(res.pca$eig[1, 2], 1), "%)", sep = ""),
      y = paste("Dim2 (", round(res.pca$eig[2, 2], 1), "%)", sep = ""),
      title = paste("PCA - Biplot pour l'espèce :", parcelle)
    ) +
    # Masquer la légende
    theme(legend.position = "none")
  
  # Afficher le graphique
  graphiques[[parcelle]] <- p
}

ACP_SP <- wrap_plots(graphiques, ncol = 2)
ACP_SP


