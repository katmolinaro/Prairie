################################################################################
################################################################################
####                          Analyse des traits                            ####
################################################################################
################################################################################


################################################################################
####              Nettoyage de la syntaxe des noms d'espèces                ####  
################################################################################

# on enlève les coquilles dans la liste d'espèces
list_sp_clean <- list_sp_clean %>%
  distinct() %>%
  mutate(
    species_clean = str_trim(species), # on enlève les espaces avant/après le 
    # nom
    species_clean = str_squish(species_clean), # on réduit le nombre d'espaces
    # consécutifs à 1
    species_clean = tolower(species_clean), # on met tout en minuscule
    species_clean = stringr::str_to_sentence(species_clean), # on met la 1ère
    # lettre en majuscule 
    species_clean = str_replace_all(species_clean, "_", " ") # on remplace les 
    # underscores par des espaces
  )


list_sp_clean %>%
  filter(species != species_clean)
# on voit qu'il y a eu 4 lignes pour lesquelles le nettoyage était nécessaire

# on va enlever aussi les (= _), les inconnus et autres incohérences dans sp_ID 
# et species
list_sp_clean <- list_sp_clean %>%
  mutate(
    species_clean = case_when(
      species_clean == "Lolium multiflorum (=Lolium italicum)" ~ "Lolium multiflorum",
      species_clean == "Lolium arundinaceum (=Festuca arundinacea)" ~ "Lolium arundinaceum",
      species_clean == "Trifolium dubium (=trifolium campestre)" ~ "Trifolium dubium",
      species_clean == "Astéracée indéterminée" ~ "Asteracea sp.",
      species_clean == "Brasicacea sp. (non identifiée)" ~ "Brasicacea sp.",
      species_clean == "Bromus hordaceus (=mollis)" ~ "Bromus hordaceus",
      species_clean == "Cinosorus cristatus" ~ "Cynosorus cristatus",
      species_clean == "Inconnu" ~ NA,
      species_clean == "Lathyrus sp" ~ "Lathyrus sp.",
      species_clean == "Brasicacea sp." ~ "Brassicacea sp.",
      species_clean == "Poacee inconnue" ~ "Poa sp.",
      TRUE ~ species_clean),
    sp_ID = case_when(
      sp_ID == "Lathyrus ochrus" ~ "Latoch",
      sp_ID == "Lolaru (=Fesaru)" ~ "Lolaru",
      sp_ID == "Grasp" ~ "Poasp",
      TRUE ~ sp_ID
    )
  )

# on va utiliser le package TNRS pour standardiser le nom des espèces de plantes
# qu'on a 

# pour ça, il faut un dataset qui comporte deux types de lignes, une avec les 
# indices des lignes, une avec les noms scientifiques
species_check <- data.frame(
  ID = seq_along(unique(list_sp_clean$species_clean)),
  species = unique(list_sp_clean$species_clean)
)

# on fait tourner TNRS
results <- TNRS(taxonomic_names = species_check)

# pour pouvoir faire la jointure ID de results et ID de species_check doivent 
# être au même format
species_check$ID <- as.character(species_check$ID)

# on va récupérer les noms d'espèces propres et les mettre dans species_check
species_check <- species_check %>%
  left_join(results %>% select(ID, Name_matched), 
            by = "ID"
            )

# on va renommer la colonne des noms validés proprement
species_check <- species_check %>%
  rename(validated_species = Name_matched)


################################################################################
####                  Extraction des données de traits                      ####  
################################################################################


################################################################################
####          Jointure entre les données de traits et les données           ####  
################################################################################




#Analyses ----




