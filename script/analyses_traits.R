
# Data




# Analyses





# Graphes 

N_parcelles <-ggplot(praire_data, aes(x = reorder(Parcelles, -elle_N, FUN = mean, na.rm = TRUE),
                                              y = elle_N,
                                              fill = Parcelles)) +
  geom_boxplot() +
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

