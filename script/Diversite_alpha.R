#######################
### Diversité alpha ###
#######################

#Indice de Shannon et Pielou pour tout le dataset
H <- diversity(prairie_sp_clean[,-(1:2)])
J <- H/log(specnumber(prairie_sp_clean))

#Indice de Shannon et Pielou pour notre annee
H_26 <- diversity(prairie_sp_26[,-(1:2)])

J_26 <- H_26/log(specnumber(prairie_sp_26))

#Shannon de notre annee en fonction du type de parcelle
boxplot(H_26~prairie_sp_26$Parcelles)
ggplot(prairie_sp_26, aes(x = Parcelles, y = H_26, fill = Parcelles)) +
  geom_boxplot() + 
  labs (
    x = "Parcelles",
    y = "Indice de Shannon H"
  ) +
  theme(legend.position = "none", text = element_text(size = 8))

### ANOVA indice de Shannon en fonction des parcelles

tapply(H_26,prairie_sp_26$Parcelles,shapiro.test)
#normalité vérifiée pour tous les groupes

bartlett.test(H_26~prairie_sp_26$Parcelles)
#homoscedasticité vérifiée

div_alpha = aov(H_26~prairie_sp_26$Parcelles)
anova(div_alpha)
summary(div_alpha)
TukeyHSD(div_alpha)
plot(TukeyHSD(div_alpha))
#effet significatif de la prairie. Gite, Grande Prairie et Mesnil ne sont pas différentes entre elles, Chemin blanc, Mesnil 2 et Parcelle regeneree ne sont pas différentes entre elles, mais ces 2 groupes sont significativement différents entre eux. => regarder le traitement pour savoir quel est l'effet du traitement sur la diversité alpha et si c'est lié aux parcelles. 

### ANOVA indice de Pielou en fonction des parcelles

boxplot(J_26~prairie_sp_26$Parcelles)
ggplot(prairie_sp_26, aes(x = Parcelles, y = J_26, fill = Parcelles)) +
  geom_boxplot() + 
  labs (
    x = "Parcelles",
    y = "Indice de Pielou J"
  ) +
  theme(legend.position = "none", text = element_text(size = 8))

tapply(exp(J_26),prairie_sp_26$Parcelles, shapiro.test)
#normalité vérifiée

bartlett.test(exp(J_26)~prairie_sp_26$Parcelles)
#homoscedasticité pas vérifiée... 

J_parcelles = aov(exp(J_26)~prairie_sp_26$Parcelles)
anova(J_parcelles)
summary(J_parcelles)
plot(J_parcelles)
#Différence significative entre les groupes, effet significative de la prairie.
TukeyHSD(J_parcelles)
