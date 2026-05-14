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
  species_clean = unique(list_sp_clean$species_clean)
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

# on va joindre les noms validés proprement aux noms de code
list_sp_clean <- list_sp_clean %>%
  left_join(species_check %>% select(species_clean, validated_species), 
            by = "species_clean"
  )

################################################################################
####                    Formattage du jeu de données                        ####  
################################################################################

# on va juste extraire les manips pour les données qui concernent la collecte de
# 2026
prairie_sp_2026 <-prairie_sp_clean %>%
  filter (Annee == 2026)

# et on veut garder que les espèces qu'on a pu observer pendant cette collecte
# donc on désigne les colonnes species qu'on va devoir filtrer 
col_species <- names(prairie_sp_2026)[
  !(names(prairie_sp_2026) %in% c("Annee", "Parcelles"))
]

# on filtre les colonnes pour lesquelles il n'y a que des valeurs nulles
col_species_keep <- col_species[
  colSums(prairie_sp_2026[col_species]) > 0
]

# on garde que les espèces observées en 2026
prairie_sp_2026 <- prairie_sp_2026 %>%
  select(Annee, Parcelles, all_of(col_species_keep))


# on va créer une colonne pour l'id des quadrats
prairie_sp_2026 <- prairie_sp_2026 %>%
  mutate(quad_ID = row_number())

# on va ré-agencer les colonnes dans le bon ordre à nouveau
prairie_sp_2026 <- prairie_sp_2026[, c(
  "Annee",
  "Parcelles",
  "quad_ID",
  "Agrcap", "Ajurep","Alopra","Antodo","Areela",
  "Belper","Bisoff","Brasp","Brohor",
  "Capbur","Carhir","Carlep","Carpra","Cenjac","Cerfon","Cirsp","Convarv",
  "Dacglo",
  "Equarv",
  "Lolaru (=Fesaru)",
  "Fessp",
  "Galmol","Galver",
  "Hollan",
  "Juncus (=Junsp)",
  "Latpra","Leoaut","Leuvul","Lolper","Lotcor","Luzcam",
  "Matcha","Mousse","Myodis",
  "Plalan","Plamaj","Poaann","Poapra","Poatri","Polavi","Potrep",
  "Ranacr","Ranbul","Ranrep","Rubsp","Rumace",
  "Taroff","Tridub","Tripra","Trirep",
  "Veragr","Verarv","vercha","Vichir","Vicsat",
  "Silflo"
  )]

# maintenant on va pivoter le tableau pour avoir une ligne par espèce pour 
# chaque quadrat
prairie_sp_2026_long <- prairie_sp_2026 %>%
  pivot_longer(
    cols = -c(Annee, Parcelles, quad_ID),
    names_to = "species",
    values_to = "abundance"
  )

# on va joindre les noms validés proprement aux noms de code
prairie_sp_2026_long <- prairie_sp_2026_long %>%
  left_join(list_sp_clean %>% select(sp_ID, validated_species), 
            by = c("species" = "sp_ID")
  )

# on va renommer les colonnes 
prairie_sp_2026_long <- prairie_sp_2026_long %>%
  rename(
    year = Annee,
    zone = Parcelles,
    sp_ID = species,
    species = validated_species
  )

# et les réagencer pour s'y retrouver
prairie_sp_2026_long <- prairie_sp_2026_long[, c(
  "year",
  "zone",
  "quad_ID",
  "sp_ID",
  "species",
  "abundance"
  )]

# on fait quelques petits changements de noms d'espèces pour les plus chiantes
prairie_sp_2026_long <- prairie_sp_2026_long %>%
  mutate(
    species = case_when(
      species == "Leontodon autumnalis" ~ "Scorzoneroides autumnalis",
      species == "Taraxacum officinale" ~ "Taraxacum officinale agg.",
      species == "Bistorta officinalis" ~ "Persicaria bistorta",
      species == "Matricaria chamomilla" ~ "Matricaria recutita",
      TRUE ~ species
      )
    )


################################################################################
####              Extraction des données de traits spécifiques              ####  
################################################################################



#test avec un vecteur

# list <- pull(.data = list_sp, var = -1)
# list
# 
# ell <- c("ell_light_uk", "ell_moist_uk", "ell_pH_uk", "ell_N", "ell_S")
# 
# test <- tr8(species_list = c("Poa annua","Holcus lanatus"), download_list = ell, allow_persistent = T)
# 
# as_tibble(test@results, rownames = "sp")
# 
# # as_tibble(test@results, rownames = "sp") %>% mutate(ell_light_uk = strsplit(ell_light_uk, ";"))
# # as_tibble(test@results, rownames = "sp") %>% mutate(ell_light_uk = lapply(strsplit(ell_light_uk, ";"),as.numeric))
# # as_tibble(test@results, rownames = "sp") %>% mutate(ell_light_uk = unlist(lapply(strsplit(ell_light_uk, ";"),as.numeric)) )
# 
# 
# as_tibble(test@results, rownames = "sp") %>% mutate(ell_light_uk = mean(unlist(lapply(strsplit(ell_light_uk, ";"),as.numeric))))
# as_tibble(test@results, rownames = "sp") %>% mutate(ell_pH_uk = mean(unlist(lapply(strsplit(gsub("x","",ell_pH_uk), ";"),as.numeric)),na.rm=T))
# 
# # Script pour sortir le tableau de results de tr8 et hop
# 
# test_table <- as_tibble(test@results, rownames = "sp") %>% rowwise() %>%
#   mutate(ell_light_uk = mean(unlist(lapply(strsplit(gsub("x","",ell_light_uk), ";"),as.numeric)),na.rm=T)) %>%
#   mutate(ell_pH_uk = mean(unlist(lapply(strsplit(gsub("x","",ell_pH_uk), ";"),as.numeric)),na.rm=T)) %>%
#   mutate(ell_moist_uk = mean(unlist(lapply(strsplit(gsub("x","",ell_moist_uk), ";"),as.numeric)),na.rm=T)) %>%
#   mutate(ell_N = mean(unlist(lapply(strsplit(gsub("x","",ell_N), ";"),as.numeric)),na.rm=T)) %>%
#   mutate(ell_S = mean(unlist(lapply(strsplit(gsub("x","",ell_S), ";"),as.numeric)),na.rm=T))
# 
# test_table <- as_tibble(test@results, rownames = "sp") %>% rowwise() %>%
#   mutate(ell_light_uk = mean(unlist(lapply(strsplit(gsub("x","",ell_light_uk), ";"),as.numeric)),na.rm=T),
#          ell_pH_uk = mean(unlist(lapply(strsplit(gsub("x","",ell_pH_uk), ";"),as.numeric)),na.rm=T),
#          ell_moist_uk = mean(unlist(lapply(strsplit(gsub("x","",ell_moist_uk), ";"),as.numeric)),na.rm=T), 
#          ell_N = mean(unlist(lapply(strsplit(gsub("x","",ell_N), ";"),as.numeric)),na.rm=T), 
#          ell_S = mean(unlist(lapply(strsplit(gsub("x","",ell_S), ";"),as.numeric)),na.rm=T))









#Nous avons species_check$validated_species pour la liste d'espèces
#Pour la liste de données à récup :

ell <- c("ell_light_uk", "ell_moist_uk", "ell_pH_uk", "ell_N", "ell_S")

# On récupère nos données :

#traits <- tr8(species_list = species_check$validated_species, download_list = ell, allow_persistent = T)

#pb avec 2 esp, je les retire pour voir si pb reglé : + RETIRE Carex vulpina car 
# NA en species heck !!!

species_check_rmv <- species_check %>%
  filter_out(validated_species == c("Cynosurus cristatus", "Myosotis")) %>%
  filter_out(species_clean == "Carex vulpina") %>%
  mutate(
    validated_species = case_when(
      validated_species == "Leontodon autumnalis" ~ "Scorzoneroides autumnalis",
      validated_species == "Taraxacum officinale" ~ "Taraxacum officinale agg.",
      validated_species == "Bistorta officinalis" ~ "Persicaria bistorta",
      validated_species == "Centaurea jacea" ~ "Centaurea x moncktonii = C. nigra x C. jacea",
      validated_species == "Matricaria chamomilla" ~ "Matricaria recutita",
      TRUE ~ validated_species
    )
  )
# ça marche pour la chamomille et bistorta mais le reste naur sadddd

#On re test

traits <- tr8(species_list = species_check_rmv$validated_species, download_list = ell, allow_persistent = T)

# traits_test <- tr8(species_list = species_check_rmv$validated_species[1:35], download_list = ell, synonyms = FALSE, allow_persistent = TRUE)
# print(traits_test)


trait_table <- as_tibble(traits@results, rownames = "sp") %>% rowwise() %>%
  mutate(ell_light_uk = mean(unlist(lapply(strsplit(gsub("x","",ell_light_uk), ";"),as.numeric)),na.rm=T)) %>%
  mutate(ell_pH_uk = mean(unlist(lapply(strsplit(gsub("x","",ell_pH_uk), ";"),as.numeric)),na.rm=T)) %>%
  mutate(ell_moist_uk = mean(unlist(lapply(strsplit(gsub("x","",ell_moist_uk), ";"),as.numeric)),na.rm=T)) %>%
  mutate(ell_N = mean(unlist(lapply(strsplit(gsub("x","",ell_N), ";"),as.numeric)),na.rm=T)) %>%
  mutate(ell_S = mean(unlist(lapply(strsplit(gsub("x","",ell_S), ";"),as.numeric)),na.rm=T))

 #trait_table[161,] = list("Lolium arundinaceum", 7,7,6,6,1)
trait_table[34,] = list("Centaurea jacea", 7,6,5,5,0)
trait_table[2,] = list("Achillea millefollium", 7,5,6,4,1)
trait_table[18,] = list("Bromopsis erecta", 7,4,8,3,0)
trait_table[58,] = list("Festuca pratensis", 7,6,6,6,0)
trait_table[91,] = list("Lotus uliginosus", 7,8,6,4,0)
trait_table[94,] = list("Lychnis flo-cuculi", 7,9,6,4,0)
trait_table[126,] = list("Rumex acutus", 5,7,7,7,0)

################################################################################
####          Jointure entre les données de traits et les données           ####  
################################################################################

prairie_sp_2026_long <- prairie_sp_2026_long %>%
 left_join(trait_table %>% select(sp, ell_light_uk, ell_moist_uk, ell_pH_uk, ell_N, ell_S),
           by = c("species" = "sp"))















# on va créer un dataset ppur l'année 2019
prairie_sp_2019 <- prairie_sp_clean %>%
  filter(Annee == 2019)

# et on veut garder que les espèces qu'on a pu observer pendant cette collecte
# donc on désigne les colonnes species qu'on va devoir filtrer 
col_species <- names(prairie_sp_2019)[
  !(names(prairie_sp_2019) %in% c("Annee", "Parcelles"))
]

# on filtre les colonnes pour lesquelles il n'y a que des valeurs nulles
col_species_keep <- col_species[
  colSums(prairie_sp_2019[col_species]) > 0
]

# on garde que les espèces observées en 2019
prairie_sp_2019 <- prairie_sp_2019 %>%
  select(Annee, Parcelles, all_of(col_species_keep))


# on va créer une colonne pour l'id des quadrats
prairie_sp_2019 <- prairie_sp_2019 %>%
  mutate(quad_ID = row_number())

# on va ré-agencer les colonnes dans le bon ordre à nouveau
prairie_sp_2019 <- prairie_sp_2019[, c(
  "Annee",
  "Parcelles",
  "quad_ID",
  "Achmil",  "Ajurep",  "Alopra",  "Antodo", "Astsp",   "Belper",  "Broere",
  "Brohor",  "Broram",  "Capbur",  "Carhir" , "Cenjac",  "Cerfon",  "Cirsp" , 
  "Convarv", "Crecap",  "Cyncri",  "Dacglo",  "Daucar",  "Fespra" , "Galmol" ,
  "Galver",  "Gerdis",  "Hiesp" ,  "Hollan",  "Latpra" , "Latsp" ,  "Leuvul" ,
  "Lolper",  "Lotcor",  "Lotuli",  "Luzcam",  "Lycflo"  ,"Lysnum",  "Lysvul" ,
  "Mousse",  "Phlpra",  "Plamaj",  "Poaann",  "Poapra",  "Poatri" , "Potrep" ,
  "Pruspi",  "Ranacr",  "Ranbul",  "Ranrep",  "Rhimin" , "Rumacu",  "Rumobt" ,
  "Salsp" ,  "Stehos",  "Trapra" , "Tridub",  "Tripra"  ,"Trirep"  ,"Veralc" ,
  "vercha",  "Veroff",  "Vicbis" , "Vicsat",  "Vicsep",  "Lotped" 
)]


# maintenant on va pivoter le tableau pour avoir une ligne par espèce pour 
# chaque quadrat
prairie_sp_2019_long <- prairie_sp_2019 %>%
  pivot_longer(
    cols = -c(Annee, Parcelles, quad_ID),
    names_to = "species",
    values_to = "abundance"
  )

# on va joindre les noms validés proprement aux noms de code
prairie_sp_2019_long <- prairie_sp_2019_long %>%
  left_join(list_sp_clean %>% select(sp_ID, validated_species), 
            by = c("species" = "sp_ID")
  )

# on va renommer les colonnes 
prairie_sp_2019_long <- prairie_sp_2019_long %>%
  rename(
    year = Annee,
    zone = Parcelles,
    sp_ID = species,
    species = validated_species
  )

# et les réagencer pour s'y retrouver
prairie_sp_2019_long <- prairie_sp_2019_long[, c(
  "year",
  "zone",
  "quad_ID",
  "sp_ID",
  "species",
  "abundance"
  )]

# # on filtre les colonnes pour lesquelles il n'y a que des valeurs nulles
# col_species_keep <- col_species[
#   colSums(prairie_sp_2019[col_species]) > 0
# ]
# 
# # on garde que les espèces observées en 2019
# prairie_sp_2019 <- prairie_sp_2019 %>%
#   select(Annee, Parcelles, all_of(col_species_keep))
# 
# 
# # on va créer une colonne pour l'id des quadrats
# prairie_sp_2019 <- prairie_sp_2019 %>%
#   mutate(quad_ID = row_number())


# # maintenant on va pivoter le tableau pour avoir une ligne par espèce pour 
# # chaque quadrat
# prairie_sp_2019_long <- prairie_sp_2019 %>%
#   pivot_longer(
#     cols = -c(Annee, Parcelles, quad_ID),
#     names_to = "species",
#     values_to = "abundance"
#   )

# on fait quelques petits changements de noms d'espèces pour les plus chiantes
# prairie_sp_2019_long <- prairie_sp_2019_long %>%
#   mutate(
#     species = case_when(
#       species == "Leontodon autumnalis" ~ "Scorzoneroides autumnalis",
#       species == "Taraxacum officinale" ~ "Taraxacum officinale agg.",
#       species == "Bistorta officinalis" ~ "Persicaria bistorta",
#       species == "Matricaria chamomilla" ~ "Matricaria recutita",
#       TRUE ~ species
#     )
#   ) %in% c("Annee", "Parcelles")

# # on va joindre les noms validés proprement aux noms de code
# prairie_sp_2019_long <- prairie_sp_2019_long %>%
#   left_join(list_sp_clean %>% select(sp_ID, validated_species), 
#             by = c("species" = "sp_ID")
#   )
# 
# # on va renommer les colonnes 
# prairie_sp_2019_long <- prairie_sp_2019_long %>%
#   rename(
#     year = Annee,
#     zone = Parcelles,
#     sp_ID = species,
#     species = validated_species
#   )
# 
# # et les réagencer pour s'y retrouver
# prairie_sp_2019_long <- prairie_sp_2019_long[, c(
#   "year",
#   "zone",
#   "quad_ID",
#   "sp_ID",
#   "species",
#   "abundance"
# )]
# 
# # on fait quelques petits changements de noms d'espèces pour les plus chiantes
# prairie_sp_2019_long <- prairie_sp_2019_long %>%
#   mutate(
#     species = case_when(
#       species == "Leontodon autumnalis" ~ "Scorzoneroides autumnalis",
#       species == "Taraxacum officinale" ~ "Taraxacum officinale agg.",
#       species == "Bistorta officinalis" ~ "Persicaria bistorta",
#       species == "Matricaria chamomilla" ~ "Matricaria recutita",
#       TRUE ~ species
#     )
#   )

ell_19 <- c("ell_light_uk", "ell_moist_uk", "ell_pH_uk", "ell_N", "ell_S")

species_check_rmv <- species_check %>%
  filter_out(validated_species == c("Cynosurus cristatus", "Myosotis")) %>%
  filter_out(species_clean == "Carex vulpina")
  # mutate(
  #   validated_species = case_when(
  #     validated_species == "Leontodon autumnalis" ~ "Scorzoneroides autumnalis",
  #     validated_species == "Taraxacum officinale" ~ "Taraxacum officinale agg.",
  #     validated_species == "Bistorta officinalis" ~ "Persicaria bistorta",
  #     validated_species == "Centaurea jacea" ~ "Centaurea x moncktonii = C. nigra x C. jacea",
  #     validated_species == "Matricaria chamomilla" ~ "Matricaria recutita",
  #     TRUE ~ validated_species
  #   )
  


traits_19 <- tr8(species_list = species_check_rmv$validated_species, download_list = ell_19, allow_persistent = T)

# traits_test <- tr8(species_list = species_check_rmv$validated_species[1:35], download_list = ell, synonyms = FALSE, allow_persistent = TRUE)
# print(traits_test)


trait_table_19 <- as_tibble(traits@results, rownames = "sp") %>% rowwise() %>%
  mutate(ell_light_uk = mean(unlist(lapply(strsplit(gsub("x","",ell_light_uk), ";"),as.numeric)),na.rm=T)) %>%
  mutate(ell_pH_uk = mean(unlist(lapply(strsplit(gsub("x","",ell_pH_uk), ";"),as.numeric)),na.rm=T)) %>%
  mutate(ell_moist_uk = mean(unlist(lapply(strsplit(gsub("x","",ell_moist_uk), ";"),as.numeric)),na.rm=T)) %>%
  mutate(ell_N = mean(unlist(lapply(strsplit(gsub("x","",ell_N), ";"),as.numeric)),na.rm=T)) %>%
  mutate(ell_S = mean(unlist(lapply(strsplit(gsub("x","",ell_S), ";"),as.numeric)),na.rm=T))

#trait_table[161,] = list("Lolium arundinaceum", 7,7,6,6,1)
trait_table_19[2,] = list("Achillea millefollium", 7,5,6,4,1)
trait_table_19[18,] = list("Bromopsis erecta", 7,4,8,3,0)
trait_table_19[58,] = list("Festuca pratensis", 7,6,6,6,0)
trait_table_19[91,] = list("Lotus uliginosus", 7,8,6,4,0)
trait_table_19[94,] = list("Lychnis flo-cuculi", 7,9,6,4,0)
trait_table_19[126,] = list("Rumex acutus", 5,7,7,7,0)

prairie_sp_2019_long <- prairie_sp_2019_long %>%
  left_join(trait_table_19 %>% select(sp, ell_light_uk, ell_moist_uk, ell_pH_uk, ell_N, ell_S),
            by = c("species" = "sp"))

prairie_sp_2019_long <- prairie_sp_2019_long %>% filter(zone == "Mesnil st père 2")


