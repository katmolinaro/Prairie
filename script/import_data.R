#Import data

#import raw ----

prairie_sp <- read_excel("raw/Relevés_CISE_Prairie.xlsx",
           sheet = "Relevés_espèce",
           col_names = TRUE)

site_info <- read_excel("raw/Relevés_CISE_Prairie.xlsx",
                        sheet = "Info_relevés",
                        col_names = TRUE)

list_sp <- read_excel("raw/Relevés_CISE_Prairie.xlsx",
                      sheet = "Liste_espèce",
                      col_names = TRUE)

#clean data ----
#:)

##Supprimer les deux dernière colonnes du premier tableau ----

prairie_sp_clean <- prairie_sp %>%
  select(-c((ncol(prairie_sp) - 1):ncol(prairie_sp)))

## Remplacer les NA par des 0 ----
prairie_sp_clean <- prairie_sp_clean %>%
  mutate_all(~ replace_na(.,0))

## Renommer les colonnes du second tableau ----

noms_propres <- c("Annees", "Parcelles", "Hauteur_max_(cm)", "Hauteur_moyenne_(cm)","%_sol_nu", "	Mode_de_gestion", "Humidité_1", "Humidité_2", "Humidité_3", "Taupe" )

prairie_sp_clean <- prairie_sp_clean %>%
  set_names(noms_propres)


## 






