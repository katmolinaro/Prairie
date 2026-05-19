#graph


# tableaux ----

# gde_parcelle <- prairie_sp_clean %>%
#   filter(Annee == "2026") %>%
#   filter(Parcelles == "Grande parcelle")
# 
# gde_parcelle <- gde_parcelle %>%
#   add_row(colSums(gde_parcelle [3 : ncol(gde_parcelle)]))
# 
# colSums(gde_parcelle [3 : ncol(gde_parcelle)])
# 
# summary(gde_parcelle)


gde_parcelle <- prairie_sp_2026_long %>%
  filter(Annee == "2026") %>%
  filter(Parcelles == "Grande parcelle")

gde_parcelle <- gde_parcelle %>%
  group_by(species) %>%
  summarise(sum_ab = sum(abundance))

gde_parcelle <- gde_parcelle %>%
  mutate(sum_ab/13*100) %>%
  filter_out(`sum_ab/13 * 100` == 0)


# camembert ----

ggplot(gde_parcelle, aes(x="", y=`sum_ab/13 * 100`, fill=species)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void()
