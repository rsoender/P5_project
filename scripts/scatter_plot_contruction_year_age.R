library(ggplot2)
library(mapview)
library(sf)
library(mapedit)

# Mac
load("/Users/rasmus/Library/CloudStorage/OneDrive-AalborgUniversitet/P5/P5 r/home.Rda")

# Windows
# load("/Users/flaat/OneDrive - Aalborg Universitet/P5/P5 r/home.Rda")

home <- homedata

# Removing variables with too many NAs
home_cleaned <- subset(home[home$SagsStatus %in% c("Solgt", "Endelig handel") & !home$EjdType %in% c("Fritidsgrund", "Fritidshus", "Helårsgrund"),], select = -c(Dato_AktuelUdbudPris, Ejd_AntalSoveVaerelser, Ejd_Ombygningsaar, Areal_Bolig_Commercial), !is.na(Pris_Salg))

# Area definitions
home_aalborg <- home_cleaned[home_cleaned$Postnr == 9000,]
home_aarhus <- home_cleaned[home_cleaned$Postnr == 8000,]
home_odense <- home_cleaned[home_cleaned$Postnr == 5000,]
home_copenhagen <- home_cleaned[home_cleaned$Postnr %in% c(1051:2450),] %>% mutate(Bynavn = "København C")# 1473 or 2450

#Scatter plot
ggplot(home_cleaned, aes(x = home_cleaned$Ejd_Opfoerelsesaar, y = home_cleaned$Alder)) + geom_point(size =2,shape = ".",alpha = 0.9) + theme_minimal() + xlab("Construction year") + ylab("Age") + stat_smooth(method = "lm", formula = y ~ x, geom = "smooth")
