library(vegan)
library(tidyverse)

echantillon_2026 <- prairie_sp_clean %>% filter(Annee %in% c("2026"))
#prairie_sp_26 <- prairie_sp_clean %>% filter(Annee %in% c("2026"))
echantillon_2026<- echantillon_2026[, colSums(echantillon_2026 != 0) > 0]
View(echantillon_2026)

#________________________________________________________________
samp26_Gp <- echantillon_2026 %>% filter( Parcelles %in% c("Grande parcelle"))

samp26_Gp <- samp26_Gp %>%
  select(-c(1,2))


test <- specaccum(samp26_Gp, method = "exact")
test1_Gp <- specaccum(samp26_Gp, method = "random")

#_________________________________________________________________

samp26_G <- echantillon_2026 %>% filter( Parcelles %in% c("Gite"))

samp26_G <- samp26_G %>%
  select(-c(1,2))

test1_G <- specaccum(samp26_G, method = "random")

#_________________________________________________________________

samp26_Pr <- echantillon_2026 %>% filter( Parcelles %in% c("Parcelle regeneree"))

samp26_Pr <- samp26_Pr %>%
  select(-c(1,2))

test1_Pr <- specaccum(samp26_Pr, method = "random")

#_________________________________________________________________

samp26_Cb <- echantillon_2026 %>% filter( Parcelles %in% c("Chemin blanc"))

samp26_Cb <- samp26_Cb %>%
  select(-c(1,2))

test1_Cb <- specaccum(samp26_Cb, method = "random")
#_________________________________________________________________

samp26_MsP1 <- echantillon_2026 %>% filter( Parcelles %in% c("Mesnil st père"))

samp26_MsP1 <- samp26_MsP1 %>%
  select(-c(1,2))

test1_MsP1 <- specaccum(samp26_MsP1, method = "random")
#_________________________________________________________________

samp26_MsP2 <- echantillon_2026 %>% filter( Parcelles %in% c("Mesnil st père 2"))

samp26_MsP2 <- samp26_MsP2 %>%
  select(-c(1,2))

test1_MsP2 <- specaccum(samp26_MsP2, method = "random")

#_________________________________________________________________

plot(test1_MsP2, ci.type = "line", col = "blue", lwd = 2,
      ci.lty = 1)

layout(matrix(c(1,2,3,4,5,6),2,3))
plot(test1_Gp, ylab = "Nombre d'espèces", xlab = "Nombre de sites", main = "Grande Parcelle")
plot(test1_G, ylab = "Nombre d'espèces", xlab = "Nombre de sites", main = "Gite")
plot(test1_Pr, ylab = "Nombre d'espèces", xlab = "Nombre de sites", main = "Parcelle régénérée")
plot(test1_Cb, ylab = "Nombre d'espèces", xlab = "Nombre de sites", main = "Chemin blanc")
plot(test1_MsP1, ylab = "Nombre d'espèces", xlab = "Nombre de sites", main = "Mesnil st père")
plot(test1_MsP2, ylab = "Nombre d'espèces", xlab = "Nombre de sites", main ="Mesnil st père 2")

specpool(samp26_Gp)
specpool(samp26_Pr)
specpool(samp26_G)
specpool(samp26_Cb)
specpool(samp26_MsP1)
specpool(samp26_MsP2)
