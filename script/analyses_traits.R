
### Data

prairie_sp_2026_long$new_N <- prairie_sp_2026_long$ell_N * prairie_sp_2026_long$abundance
prairie_sp_2026_long$new_pH <- prairie_sp_2026_long$ell_pH_uk* prairie_sp_2026_long$abundance
prairie_sp_2026_long$new_light <- prairie_sp_2026_long$ell_light_uk * prairie_sp_2026_long$abundance
prairie_sp_2026_long$new_sel <- prairie_sp_2026_long$ell_S * prairie_sp_2026_long$abundance
prairie_sp_2026_long$new_hum <- prairie_sp_2026_long$ell_moist_uk * prairie_sp_2026_long$abundance

prairie_sp_2026_long <- na.omit(prairie_sp_2026_long)

traits_parcelles <- prairie_sp_2026_long %>%
  group_by(zone,quad_ID) %>%
  summarise(final_N = sum(new_N),final_pH = sum(new_pH), final_sel=sum(new_sel), final_hum=sum(new_hum),
            final_light=sum(new_light)) %>%
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


#covariance des traits
trait_data <- data.frame(N = traits_parcelles$final_N,
                         light = traits_parcelles$final_light,
                         humidité = traits_parcelles$final_hum,
                         sel = traits_parcelles$final_sel,
                         pH = traits_parcelles$final_pH)

heatmap <- rcorr(as.matrix(trait_data), type="pearson") 
ggcorrplot(heatmap$r, hc.order = FALSE, 
           type = "lower", 
           lab = TRUE,
           lab_size = 8, 
           tl.cex = 19,
           method="square", 
           tl.col = "black",
           colors = c("blue", "white", "red"), 
           ggtheme=theme_bw)