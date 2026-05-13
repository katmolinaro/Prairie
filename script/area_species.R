library(vegan)
library(tidyverse)

echantillon_2026 = prairie_sp_clean %>% filter(Annee %in% c("2026"))
#prairie_sp_26 <- prairie_sp_clean %>% filter(Annee %in% c("2026"))
echantillon_2026<- echantillon_2026[, colSums(echantillon_2026 != 0) > 0]
View(echantillon_2026)


#________________________________________________________________
samp26_Gp = echantillon_2026 %>% filter( Parcelles %in% c("Grande Parcelle"))


samp26_Gp <- samp26_Gp %>%
  select(-c(1,2))


test <- specaccum(samp26_Gp, method = "exact")
test1 <- specaccum(samp26_Gp, method = "random")
test2 <- specaccum(samp26_Gp, method = "coleman")

plot(test, ylab = "Nombre d'espèces", xlab = "Nombre de sites")
plot(test1, ylab = "Nombre d'espèces", xlab = "Nombre de sites")
plot(test2, ylab = "Nombre d'espèces", xlab = "Nombre de sites")

#_________________________________________________________________

samp26_G = echantillon_2026 %>% filter( Parcelles %in% c("Gite"))


samp26_G <- samp26_G %>%
  select(-c(1,2))


test <- specaccum(samp26_G, method = "exact")
test1 <- specaccum(samp26_G, method = "random")
test2 <- specaccum(samp26_G, method = "coleman")

plot(test, ylab = "Nombre d'espèces", xlab = "Nombre de sites")
plot(test1, ylab = "Nombre d'espèces", xlab = "Nombre de sites")
plot(test2, ylab = "Nombre d'espèces", xlab = "Nombre de sites")

#_________________________________________________________________

samp26_Pr = echantillon_2026 %>% filter( Parcelles %in% c("Parcelle regeneree"))


samp26_Pr <- samp26_Pr %>%
  select(-c(1,2))


test <- specaccum(samp26_Pr, method = "exact")
test1 <- specaccum(samp26_Pr, method = "random")
test2 <- specaccum(samp26_Pr, method = "coleman")

plot(test, ylab = "Nombre d'espèces", xlab = "Nombre de sites")
plot(test1, ylab = "Nombre d'espèces", xlab = "Nombre de sites")
plot(test2, ylab = "Nombre d'espèces", xlab = "Nombre de sites")

#_________________________________________________________________

samp26_Cb = echantillon_2026 %>% filter( Parcelles %in% c("Chemin blanc"))


samp26_Cb <- samp26_Cb %>%
  select(-c(1,2))



test <- specaccum(samp26_Cb, method = "exact")
test1 <- specaccum(samp26_Cb, method = "random")
test2 <- specaccum(samp26_Cb, method = "coleman")

plot(test, ylab = "Nombre d'espèces", xlab = "Nombre de sites")
plot(test1, ylab = "Nombre d'espèces", xlab = "Nombre de sites")
plot(test2, ylab = "Nombre d'espèces", xlab = "Nombre de sites")

#_________________________________________________________________

samp26_MsP1= echantillon_2026 %>% filter( Parcelles %in% c("Mesnil st père"))


samp26_MsP1 <- samp26_MsP1 %>%
  select(-c(1,2))



test <- specaccum(samp26_MsP1, method = "exact")
test1 <- specaccum(samp26_MsP1, method = "random")
test2 <- specaccum(samp26_MsP1, method = "coleman")

plot(test, ylab = "Nombre d'espèces", xlab = "Nombre de sites")
plot(test1, ylab = "Nombre d'espèces", xlab = "Nombre de sites")
plot(test2, ylab = "Nombre d'espèces", xlab = "Nombre de sites")

#_________________________________________________________________

samp26_MsP2= echantillon_2026 %>% filter( Parcelles %in% c("Mesnil st père 2"))


samp26_MsP2 <- samp26_MsP2 %>%
  select(-c(1,2))



test <- specaccum(samp26_MsP2, method = "exact")
test1 <- specaccum(samp26_MsP2, method = "random")
test2 <- specaccum(samp26_MsP2, method = "coleman")

plot(test, ylab = "Nombre d'espèces", xlab = "Nombre de sites")
plot(test1, ylab = "Nombre d'espèces", xlab = "Nombre de sites")
plot(test2, ylab = "Nombre d'espèces", xlab = "Nombre de sites")



#_________________________________________________________________

### Version d'origine ###


sous_dataset <- data_mock %>%
  select(-c(1,2))

test <- specaccum(sous_dataset, method = "exact")
test1 <- specaccum(data_mock, method = "random")
test2 <- specaccum(data_mock, method = "coleman")

plot(test, ylab = "Nombre d'espèces", xlab = "Nombre de sites")
plot(test1, ylab = "Nombre d'espèces", xlab = "Nombre de sites")
plot(test2, ylab = "Nombre d'espèces", xlab = "Nombre de sites")
