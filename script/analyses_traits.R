
### Data

prairie_sp_2026_long$new_N <- prairie_sp_2026_long$ell_N * prairie_sp_2026_long$abundance
prairie_sp_2026_long$new_pH <- prairie_sp_2026_long$ell_pH_uk* prairie_sp_2026_long$abundance
prairie_sp_2026_long$new_light <- prairie_sp_2026_long$ell_light_uk * prairie_sp_2026_long$abundance
prairie_sp_2026_long$new_sel <- prairie_sp_2026_long$ell_S * prairie_sp_2026_long$abundance
prairie_sp_2026_long$new_hum <- prairie_sp_2026_long$ell_moist_uk * prairie_sp_2026_long$abundance

prairie_sp_2026_long <- na.omit(prairie_sp_2026_long)

traits_parcelles <- prairie_sp_2026_long %>%
  group_by(zone,quad_ID) %>%
  summarise(final_N = sum(new_N),final_pH = sum(new_pH), final_sel=sum(new_sel), 
            final_hum=sum(new_hum),
            final_light=sum(new_light)) %>%
  ungroup()


### Analyses
#première anova pour savoir si on a des différences de traits entre les parcelles + vérification conditions application
#pour N
N <- aov(final_N~zone, data =traits_parcelles)
plot(N)
#si conditions violées > Kruskall Wallis ou transformation des données
summary(N)
TukeyHSD(N, conf.level = 0.95)

#pour lumière
lumière <- aov(final_light~zone, data =traits_parcelles)
plot(lumière)
#si conditions violées > Kruskall Wallis ou transformation des données
summary(lumière)
kruskal.test(final_light~zone, data =traits_parcelles)
TukeyHSD(lumière, conf.level = 0.95)
kruskalmc(traits_parcelles$final_light, traits_parcelles$zone)


#pour pH
pH <-  aov(final_pH~zone, data =traits_parcelles)
plot(pH)

summary(pH)
kruskal.test(final_pH~zone, data =traits_parcelles)
TukeyHSD(pH, conf.level = 0.95)

#pour humidité
humidité <- aov(final_hum~zone, data =traits_parcelles)
plot(humidité)

summary(humidité)
TukeyHSD(humidité, conf.level = 0.95)

kruskal.test(final_hum~zone, data =traits_parcelles)


#pour sel
sel <- aov(final_sel~zone, data =traits_parcelles)
plot(sel)

summary(sel)
TukeyHSD(sel, conf.level = 0.95)


res.pcatrait26 <- ade4::dudi.pca(traits_parcelles[,-(1:2)], nf = 5, scannf = F)
summary(res.pcatrait26)
#Graphes parcelles
ACP26 <- fviz_pca_ind(res.pcatrait26, habillage = traits_parcelles$zone, addEllipses = T, geom = "point", axes = 1:2)
ACP26

ACP26 <- fviz_pca_ind(res.pcatrait26, axes = 1:2)
ACP26

ACP <- fviz_pca_var(res.pcatrait26)
ACP


### Graphes 
(N_parcelles <-ggplot(traits_parcelles, aes(x = reorder(zone, -final_N, FUN = mean, na.rm = TRUE),
                                              y = final_N,
                                              fill = zone)) +
  geom_boxplot(aes(color = zone)) +
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
)


ggsave("N_parcelles.svg",N_parcelles,width=250,height=200,units=c("mm"),dpi=900,bg="transparent",limitsize = FALSE)




(light_parcelles <-ggplot(traits_parcelles, aes(x = reorder(zone, -final_light, FUN = mean, na.rm = TRUE),
                                           y = final_light,
                                           fill = zone)) +
  geom_boxplot(aes(color = zone)) +
  geom_jitter(size = 1) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.75, color = "black", size = 0.2,linetype = "dashed") +
  theme_minimal() +
  #stat_summary(fun = max, geom = "text", aes(label = c(new_letters_Dim1)[factor(SP_valide)]), vjust = -0.5, size = 7) +
  labs(x = "",
       y = "Valeurs d'affinité à la lumière") +
  theme(axis.text.x = element_text(angle = 70, hjust = 1, vjust=1, size = 14),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16, face = "bold"),
        legend.position = "none")
)
light_parcelles

(sel_parcelles <-ggplot(traits_parcelles, aes(x = reorder(zone, -final_sel, FUN = mean, na.rm = TRUE),
                                                y = final_sel,
                                                fill = zone)) +
    geom_boxplot(aes(color = zone)) +
    geom_jitter(size = 1) +
    stat_summary(fun = mean, geom = "crossbar", width = 0.75, color = "black", size = 0.2,linetype = "dashed") +
    theme_minimal() +
    #stat_summary(fun = max, geom = "text", aes(label = c(new_letters_Dim1)[factor(SP_valide)]), vjust = -0.5, size = 7) +
    labs(x = "",
         y = "Valeurs d'affinité au sel") +
    theme(axis.text.x = element_text(angle = 70, hjust = 1, vjust=1, size = 14),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 16, face = "bold"),
          legend.position = "none")
)
sel_parcelles

(pH_parcelles <-ggplot(traits_parcelles, aes(x = reorder(zone, -final_pH, FUN = mean, na.rm = TRUE),
                                                y = final_pH,
                                                fill = zone)) +
    geom_boxplot(aes(color = zone)) +
    geom_jitter(size = 1) +
    stat_summary(fun = mean, geom = "crossbar", width = 0.75, color = "black", size = 0.2,linetype = "dashed") +
    theme_minimal() +
    #stat_summary(fun = max, geom = "text", aes(label = c(new_letters_Dim1)[factor(SP_valide)]), vjust = -0.5, size = 7) +
    labs(x = "",
         y = "Valeurs d'affinité au pH") +
    theme(axis.text.x = element_text(angle = 70, hjust = 1, vjust=1, size = 14),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 16, face = "bold"),
          legend.position = "none")
)
pH_parcelles

(hum_parcelles <-ggplot(traits_parcelles, aes(x = reorder(zone, -final_hum, FUN = mean, na.rm = TRUE),
                                             y = final_hum,
                                             fill = zone)) +
    geom_boxplot(aes(color = zone)) +
    geom_jitter(size = 1) +
    stat_summary(fun = mean, geom = "crossbar", width = 0.75, color = "black", size = 0.2,linetype = "dashed") +
    theme_minimal() +
    #stat_summary(fun = max, geom = "text", aes(label = c(new_letters_Dim1)[factor(SP_valide)]), vjust = -0.5, size = 7) +
    labs(x = "",
         y = "Valeurs d'affinité à l'humidité") +
    theme(axis.text.x = element_text(angle = 70, hjust = 1, vjust=1, size = 14),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 16, face = "bold"),
          legend.position = "none")
)
hum_parcelles

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














####################### Analyses temp



### Data

traits_parcelles <- traits_parcelles %>% filter(zone =="Mesnil st père 2")

prairie_sp_2019_long$new_N <- prairie_sp_2019_long$ell_N * prairie_sp_2019_long$abundance
prairie_sp_2019_long$new_pH <- prairie_sp_2019_long$ell_pH_uk* prairie_sp_2019_long$abundance
prairie_sp_2019_long$new_light <- prairie_sp_2019_long$ell_light_uk * prairie_sp_2019_long$abundance
prairie_sp_2019_long$new_sel <- prairie_sp_2019_long$ell_S * prairie_sp_2019_long$abundance
prairie_sp_2019_long$new_hum <- prairie_sp_2019_long$ell_moist_uk * prairie_sp_2019_long$abundance

prairie_sp_2019_long <- na.omit(prairie_sp_2019_long)

traits_parcelles_19 <- prairie_sp_2019_long %>%
  group_by(zone,quad_ID) %>%
  summarise(final_N = sum(new_N),final_pH  = sum(new_pH), final_sel =sum(new_sel), 
            final_hum =sum(new_hum),
            final_light =sum(new_light)) %>%
  ungroup()

traits_parcelles <- traits_parcelles %>%
  mutate(year = 2026)

traits_parcelles_19 <- traits_parcelles_19 %>%
  mutate(year = 2019)

traits_parcelles_20192026 <- bind_rows(traits_parcelles, traits_parcelles_19)



### Analyses
#première anova pour savoir si on a des différences de traits entre les parcelles + vérification conditions application
#pour N
t.test(traits_parcelles$final_N_26, traits_parcelles_19$final_N_19)


#pour lumière
t.test(traits_parcelles$final_light_26, traits_parcelles_19$final_light_19)


#pour pH
t.test(traits_parcelles$final_pH_26, traits_parcelles_19$final_pH_19)


#pour humidité
t.test(traits_parcelles$final_hum_26, traits_parcelles_19$final_hum_19)


#pour sel
t.test(traits_parcelles$final_sel_26, traits_parcelles_19$final_sel_19)




### Graphes 



par(mfrow=c(2,2))

(N_parcelles <-ggplot(traits_parcelles_20192026, aes(x = reorder(year, final_N, FUN = mean, na.rm = TRUE, order = F),
                                            y = final_N
                                            )) +
    geom_boxplot() +
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
)


ggsave("N_parcelles.svg",N_parcelles,width=250,height=200,units=c("mm"),dpi=900,bg="transparent",limitsize = FALSE)




(light_parcelles <-ggplot(traits_parcelles_20192026, aes(x = reorder(year, final_light, FUN = mean, na.rm = TRUE),
                                                y = final_light
                                                )) +
    geom_boxplot() +
    geom_jitter(size = 1) +
    stat_summary(fun = mean, geom = "crossbar", width = 0.75, color = "black", size = 0.2,linetype = "dashed") +
    theme_minimal() +
    #stat_summary(fun = max, geom = "text", aes(label = c(new_letters_Dim1)[factor(SP_valide)]), vjust = -0.5, size = 7) +
    labs(x = "",
         y = "Valeurs d'affinité à la lumière") +
    theme(axis.text.x = element_text(angle = 70, hjust = 1, vjust=1, size = 14),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 16, face = "bold"),
          legend.position = "none")
)
light_parcelles


(pH_parcelles <-ggplot(traits_parcelles_20192026, aes(x = reorder(year, final_pH, FUN = mean, na.rm = TRUE),
                                             y = final_pH
                                             )) +
    geom_boxplot() +
    geom_jitter(size = 1) +
    stat_summary(fun = mean, geom = "crossbar", width = 0.75, color = "black", size = 0.2,linetype = "dashed") +
    theme_minimal() +
    #stat_summary(fun = max, geom = "text", aes(label = c(new_letters_Dim1)[factor(SP_valide)]), vjust = -0.5, size = 7) +
    labs(x = "",
         y = "Valeurs d'affinité au pH") +
    theme(axis.text.x = element_text(angle = 70, hjust = 1, vjust=1, size = 14),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 16, face = "bold"),
          legend.position = "none")
)
pH_parcelles

(hum_parcelles <-ggplot(traits_parcelles_20192026, aes(x = reorder(year, final_hum, FUN = mean, na.rm = TRUE),
                                              y = final_hum
                                              )) +
    geom_boxplot() +
    geom_jitter(size = 1) +
    stat_summary(fun = mean, geom = "crossbar", width = 0.75, color = "black", size = 0.2,linetype = "dashed") +
    theme_minimal() +
    #stat_summary(fun = max, geom = "text", aes(label = c(new_letters_Dim1)[factor(SP_valide)]), vjust = -0.5, size = 7) +
    labs(x = "",
         y = "Valeurs d'affinité à l'humidité") +
    theme(axis.text.x = element_text(angle = 70, hjust = 1, vjust=1, size = 14),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 16, face = "bold"),
          legend.position = "none")
)
hum_parcelles




par(mfrow=c(2,2))
light_parcelles
N_parcelles
pH_parcelles
hum_parcelles
par(mfrow=c(1,1))

layout (matrix(c(1,2,3,4),2,2))
light_parcelles
N_parcelles
pH_parcelles
hum_parcelles



