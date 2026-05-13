
###

# filtre année
prairie_sp_26 <- prairie_sp_clean %>% filter(Annee %in% c("2026"))

# filtre parcelles
prairie_sp_ACP <- prairie_sp_26 %>% filter(Parcelles %in% c("Grande parcelle","Chemin blanc", "Gite", "Parcelle regeneree", "Mesnil st père", "Mesnil st père 2"))

# filtre colonnes vides
prairie_sp_ACP <- prairie_sp_ACP[, colSums(prairie_sp_ACP != 0) > 0]



# Réaliser l'ACP
res.pca <- ade4::dudi.pca(prairie_sp_ACP[,-(1:2)], nf = 5, scannf = F)
summary(res.pca)

#Graphes parcelles
ACP <- fviz_pca_ind(res.pca,habillage = prairie_sp_ACP$Parcelles, addEllipses = TRUE, geom = "point")
?fviz_pca_ind
ACP



#Test stats diff. parcelles
randtest(bca(res.pca,fac = as.factor(prairie_sp_ACP$Parcelles), nf = 4, scannf = F))






























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


