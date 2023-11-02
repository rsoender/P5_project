library(ggplot2)
library(mapview)
library(sf)
library(mapedit)

# Mac
load("/Users/rasmus/Library/CloudStorage/OneDrive-AalborgUniversitet/P5/P5 r/home.Rda")

# Windows
# load("/Users/flaat/OneDrive - Aalborg Universitet/P5/P5 r/home.Rda")

home <- homedata
home_flat <- home[home$EjdType == "Ejerlejlighed", ]

# Removing variables with too many NAs
home_cleaned <- subset(home[home$SagsStatus %in% c("Solgt", "Endelig handel") & !home$EjdType %in% c("Fritidsgrund", "Fritidshus", "Helårsgrund"),], select = -c(Dato_AktuelUdbudPris, Ejd_AntalSoveVaerelser, Ejd_Ombygningsaar, Areal_Bolig_Commercial), !is.na(Pris_Salg))

# Area definitions
home_aalborg <- home_cleaned[home_cleaned$Postnr == 9000,]
home_aarhus <- home_cleaned[home_cleaned$Postnr == 8000,]
home_odense <- home_cleaned[home_cleaned$Postnr == 5000,]
home_copenhagen <- home_cleaned[home_cleaned$Postnr %in% c(1051:2450),] %>% mutate(Bynavn = "København C")# 1473 or 2450

# Scatter plot
ggplot(home_flat, aes(x = home_flat$Dist_raadhus, y = home_flat$Pris_Salg)) +
  geom_point(col = "blue", size = 2, shape = ".", alpha = 0.9) +
  geom_point(aes(x = home_flat$Dist_skole), col = "red", size = 2, shape = ".", alpha = 0.9)

home_flat <- subset(home_flat, !is.na(Areal_Kaelder))

nrow(home_flat[home_flat$Hoejhus == 1,])

home_cleaned_temp <- subset(home_cleaned, !is.na(Alder) & !is.na(Ejd_Opfoerelsesaar))

nrow(home_cleaned[home_cleaned$Ejd_Opfoerelsesaar,])
nrow(home_cleaned[home_cleaned$Alder,])
sum(is.na(home_cleaned[home_cleaned$Pris_Salg,]))
nrow(home_cleaned[home_cleaned_temp$Ejd_Opfoerelsesaar == 1801, ])
