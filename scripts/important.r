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

# Cleaning
home_cleaned <- subset(home[home$SagsStatus %in% c("Solgt", "Endelig handel") & home$DWID_Projektsalg == "Nej" & !home$EjdType %in% c("Fritidsgrund", "Fritidshus", "Helårsgrund"),], select = -c(Dato_AktuelUdbudPris, Ejd_AntalSoveVaerelser, Ejd_Ombygningsaar, Areal_Bolig_Commercial, Ejd_Opfoerelsesaar), !is.na(Pris_Salg))

# Cleaning RegionNavn ukendt
home_cleaned <- home_cleaned[!home_cleaned$RegionNavn == "Ukendt",]

# Cleaning floor number
home_cleaned$Adresse_Etage <- ifelse(
  is.na(home_cleaned$Adresse_Etage) | grepl("(st.|ST|0|NULL)", home_cleaned$Adresse_Etage), 
  1, 
  home_cleaned$Adresse_Etage
)

home_cleaned$Adresse_Etage <- gsub("(kld.|kl)", 0, gsub("3.", 3, home_cleaned$Adresse_Etage))

# Test
table(home_cleaned$KommuneNavn)

colSums(is.na(home))

table(home_cleaned$SagsType)

# Flats only
home_flat <- home_cleaned[home_cleaned$EjdType == "Ejerlejlighed", ]

# Area definitions
home_aalborg <- home_cleaned[home_cleaned$Postnr == 9000,]
home_aarhus <- home_cleaned[home_cleaned$Postnr == 8000,]
home_odense <- home_cleaned[home_cleaned$Postnr == 5000,]
home_copenhagen <- home_cleaned[home_cleaned$Postnr %in% c(1051:2450),] %>% mutate(Bynavn = "København C")# 1473 or 2450

# Map plot
# Aalborg
home_aalborg <- st_as_sf(home_aalborg, coords = c("GisX_Wgs84", "GisY_Wgs84"), crs = 4269)
mapview(home_aalborg, map.types = "OpenStreetMap")

# Aarhus
home_aarhus <- st_as_sf(home_aarhus, coords = c("GisX_Wgs84", "GisY_Wgs84"), crs = 4269)
mapview(home_aarhus, map.types = "OpenStreetMap")

# Odense
home_odense <- st_as_sf(home_odense, coords = c("GisX_Wgs84", "GisY_Wgs84"), crs = 4269)
mapview(home_odense, map.types = "OpenStreetMap")

# Copenhagen
home_copenhagen <- st_as_sf(home_copenhagen, coords = c("GisX_Wgs84", "GisY_Wgs84"), crs = 4269)
mapview(home_copenhagen, map.types = "OpenStreetMap")

# Using only flats
home_flat <- home_cleaned[home_cleaned$EjdType == "Ejerlejlighed", ]
