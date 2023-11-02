library(ggplot2)
library(mapview)
library(sf)
library(mapedit)
library(dplyr)

# Mac
load("/Users/rasmus/Library/CloudStorage/OneDrive-AalborgUniversitet/P5/P5 r/home.Rda")

# Windows
# load("/Users/flaat/OneDrive - Aalborg Universitet/P5/P5 r/home.Rda")

home <- homedata

# Removing variables with too many NAs + rows with Fritidsgrund/-hus and Helår
home_cleaned <- subset(home[home$SagsStatus %in% c("Solgt", "Endelig handel") & !home$EjdType %in% c("Fritidsgrund", "Fritidshus", "Helårsgrund"),], select = -c(Dato_AktuelUdbudPris, Ejd_AntalSoveVaerelser, Ejd_Ombygningsaar, Areal_Bolig_Commercial), !is.na(Pris_Salg))

home_aalborg <- home_cleaned[home_cleaned$Postnr == 9000,]
home_aarhus <- home_cleaned[home_cleaned$Postnr == 8000,]
home_odense <- home_cleaned[home_cleaned$Postnr == 5000,]
home_copenhagen <- home_cleaned[home_cleaned$Postnr %in% c(1051:2450),] %>% mutate(Bynavn = "København C")# 1473 or 2450

home_it <- rbind(home_aalborg, home_aarhus, home_odense, home_copenhagen)

quantile(home_it$Pris_Salg, probs = c(1/3, 2/3))

price_levels <- matrix(cut(home_it$Pris_Salg, breaks = c(0, 1995000, 3450000, Inf), labels = c("low", "avg", "high"), include.lowest = TRUE), ncol = 1)
colnames(price_levels) <- c("price_level")
home_it <- cbind(home_it, price_levels)

home_it_table <- table(home_it$Bynavn, home_it$price_level)
home_it_table

chisq.test(home_it_table)
