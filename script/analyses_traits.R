
### Data

data$newN <- ellen_N * abondance
data$newpH <- ellen_pH* abondance
data$newLum <- ellen_Lum * abondance
data$newSel <- ellen_Sel * abondance
data$newHum <- ellen_Hum * abondance

traits_parcelles <- data %>%
  group_by(Parcelles) %>%
  group_by(Quadrat) %>%
  mutate(finalN = sum(newN)) %>%
  ungroup()

traits_parcelles$finalpH <- data %>%
  group_by(Parcelles) %>%
  group_by(Quadrat) %>%
  mutate(finalpH = sum(newN)) %>%
  ungroup()

traits_parcelles$finalLum <- data %>%
  group_by(Parcelles) %>%
  group_by(Quadrat) %>%
  mutate(finalLum = sum(newLum)) %>%
  ungroup()

traits_parcelles$finalSel <- data %>%
  group_by(Parcelles) %>%
  group_by(Quadrat) %>%
  mutate(finalSel = sum(newSel)) %>%
  ungroup()

traits_parcelles$finalHum <- data %>%
  group_by(Parcelles) %>%
  group_by(Quadrat) %>%
  mutate(finalHum = sum(newHum)) %>%
  ungroup()

### Analyses
#première anova pour savoir si on a des différences de traits entre les parcelles + vérification conditions application
#pour N
N <- aov(N_par_parcelle~parcelle, data =)
plot(N)
bartlett.test(N$residuals, dataset$parcelle)
#si conditions violées > Kruskall Wallis ou transformation des données
summary(N)

#pour lumière
lumière <- aov(lumière_par_parcelle~parcelle, data =)
plot(lumière)
bartlett.test(lumière$residuals, dataset$parcelle)

summary(lumière)

#pour pH
pH <- aov(pH_par_parcelle~parcelle, data =)
plot(pH)
bartlett.test(pH$residuals, dataset$parcelle)

summary(pH)

#pour humidité
humidité <- aov(humidité_par_parcelle~parcelle, data =)
plot(humidité)
bartlett.test(humidité$residuals, dataset$parcelle)

summary(humidité)

#Analyses post hoc (si on garde ANOVA, si on passe sur KS > kruskalmc(dataset$trait, dataset$parcelle, alpha = 0.05/n))
TukeyHSD(N, conf.level = 0.95)


### Graphes 
N_parcelles <-ggplot(praire_data, aes(x = reorder(Parcelles, -elle_N, FUN = mean, na.rm = TRUE),
                                              y = elle_N,
                                              fill = Parcelles)) +
  geom_boxplot(aes(color = Parcelle)) +
  geom_jitter(size = 1) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.75, color = "black", size = 0.2,linetype = "dashed") +
  theme_minimal() +
  #stat_summary(fun = max, geom = "text", aes(label = c(new_letters_Dim1)[factor(SP_valide)]), vjust = -0.5, size = 7) +
  labs(x = "",
       y = "Valeurs d'affinité aux nitrates") +
  theme(axis.text.x = element_text(angle = 70, hjust = 1, vjust=1, size = 14),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16, face = "bold"),
        legend.position = "none")

N_parcelles

ggsave("N_parcelles.svg",N_parcelles,width=250,height=200,units=c("mm"),dpi=900,bg="transparent",limitsize = FALSE)

