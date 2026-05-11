#Import data

#import raw ----

prairie_sp <- read_excel("raw/Relevés_CISE_Prairie",
           sheet = "Relevés_espèce",
           col_names = TRUE)

site_info <- read_excel("raw/Relevés_CISE_Prairie",
                        sheet = "Info_relevés",
                        col_names = TRUE)

list_sp <- read_excel("raw/Relevés_CISE_Prairie",
                      sheet = "Liste_espèce",
                      col_names = TRUE)

#clean data ----
#:)






