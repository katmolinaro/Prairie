################################################################################
################################################################################
####              Importation des jeux de données initiaux                  ####
################################################################################
################################################################################

prairie_sp <- read_excel("raw/Relevés_CISE_Prairie.xlsx",
           sheet = "Relevés_espèce",
           col_names = TRUE)
prairie_sp$Cirarv <- as.numeric(prairie_sp$Cirarv)
summary(prairie_sp)

site_info <- read_excel("raw/Relevés_CISE_Prairie.xlsx",
                        sheet = "Info_relevés",
                        col_names = TRUE)

list_sp <- read_excel("raw/Relevés_CISE_Prairie.xlsx",
                      sheet = "Liste_espèce",
                      col_names = FALSE)



################################################################################
################################################################################
####                        Nettoyage des données                           ####
################################################################################
################################################################################

################################################################################
### Tableau prairies_sp

# on supprime les deux dernières colonnes du tableau prairies_species
prairie_sp_clean <- prairie_sp %>%
  select(-c((ncol(prairie_sp) - 1):ncol(prairie_sp)))

# on remplace les NAs par des 0 ----
prairie_sp_clean <- prairie_sp_clean %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

################################################################################
### Tableau list_sp

# on supprime la dernière colonne du tableau list_sp
list_sp_clean <- list_sp %>%
  select(-ncol(list_sp))

# on renomme les colonnes de list_sp 
list_sp_clean <- list_sp_clean %>%
  rename(
    sp_ID = ...1,
    species = ...2)

################################################################################
### Tableau site_info

## on renomme les colonnes du tableau site_info
noms_propres <- c("Annees", 
                  "Parcelles", 
                  "Hauteur_max_(cm)", 
                  "Hauteur_moyenne_(cm)",
                  "%_sol_nu", 
                  "Mode_de_gestion", 
                  "Humidité_1", 
                  "Humidité_2", 
                  "Humidité_3", 
                  "Taupe"
                  )

site_info_clean <- site_info %>%
  set_names(noms_propres)



################################################################################
################################################################################
####                      Export des tableaux propres                       ####
################################################################################
################################################################################

write_xlsx(prairie_sp_clean, path = "output/prairie_sp_clean.xlsx")
write_xlsx(site_info_clean, path = "output/site_info_clean.xlsx")
write_xlsx(list_sp_clean, path = "output/list_sp_clean.xlsx")