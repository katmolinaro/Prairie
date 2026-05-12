library(vegan)
library(tidyverse)

sous_dataset <- data_mock %>%
  select(-c(1,2))

test <- specaccum(sous_dataset, method = "exact")
test1 <- specaccum(data_mock, method = "random")
test2 <- specaccum(data_mock, method = "coleman")

plot(test, ylab = "Nombre d'espèces", xlab = "Nombre de sites")
plot(test1, ylab = "Nombre d'espèces", xlab = "Nombre de sites")
plot(test2, ylab = "Nombre d'espèces", xlab = "Nombre de sites")
