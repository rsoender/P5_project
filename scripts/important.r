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
home_flat <- subset(home, (SagsStatus == "Solgt" | SagsStatus == "Endelig handel") & DWID_Projektsalg == "Nej" & EjdType == "Ejerlejlighed" & Postnr %in% c(9000, 8000, 5000, c(1050:2450)) & !is.na(Pris_Salg), select = c(Beloeb_EjerUdgift, Pris_EjdVurdering, Pris_Salg, AntalFremvisninger, AntalStatusmoederSaelger, Sag_AnnonceretNettet, Areal_Bolig, Areal_Bolig_Commercial, Adresse_Etage, Areal_GarageCarport, Areal_Grund, Areal_Kaelder, Ejd_Altan, Ejd_AntalRum, Ejd_Energimaerke, Salgstid, Salgsmaaned, Dist_skole, Alder, Hoejhus, Dist_raadhus, Sogn, corona, GisX_Wgs84, GisY_Wgs84))

colSums(is.na(home_flat))
summary(home_flat$Dist_raadhus)
table(home_flat$Sogn)

home_flat <- home_flat %>%
  group_by(Sogn) %>%
  filter(n() > 10) %>%
  ungroup()

nrow(home_flat)

# Cleaning Beloeb_EjerUdgift
home_flat$Beloeb_EjerUdgift <- abs(home_flat$Beloeb_EjerUdgift)

# Cleaning Areal_Bolig_Commercial
home_flat$Areal_Bolig_Commercial <- abs(home_flat$Areal_Bolig_Commercial)

# Cleaning floor number
home_flat$Adresse_Etage <- ifelse(
  is.na(home_flat$Adresse_Etage) | grepl("(st.|ST|0|NULL)", home_flat$Adresse_Etage), 
  1, 
  home_flat$Adresse_Etage
)

home_flat$Adresse_Etage <- gsub("(kld.|kl)", 0, gsub("3.", 3, home_flat$Adresse_Etage))

# Changing NAs to 0.
home_flat <- home_flat %>% mutate(across(c(AntalFremvisninger, AntalStatusmoederSaelger, Areal_Bolig_Commercial, Areal_GarageCarport, Areal_Grund, Areal_Kaelder), ~ifelse(is.na(.), 0, .))) 

# Area definitions
home_aalborg <- home[home$Postnr == 9000,]
home_aarhus <- home[home$Postnr == 8000,]
home_odense <- home[home$Postnr == 5000,]
home_copenhagen <- home[home$Postnr %in% c(1050:2450),] %>% mutate(Bynavn = "København C")# 1473 or 2450

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

# Test
summary(lm(Pris_Salg ~ Beloeb_EjerUdgift + Pris_EjdVurdering + AntalFremvisninger + AntalStatusmoederSaelger + Sag_AnnonceretNettet + Areal_Bolig + Areal_Bolig_Commercial + Adresse_Etage + Areal_GarageCarport + Areal_Grund + Areal_Kaelder + Ejd_Altan + Ejd_AntalRum + Ejd_Energimaerke + Salgstid + Salgsmaaned + Dist_skole + Alder + Hoejhus + Dist_raadhus + Sogn + corona, data = home_flat))

ggplot(home_flat, aes(x = home_flat$Areal_Bolig, y = home_flat$Pris_Salg)) + geom_point(size =2,shape = ".",alpha = 0.9) + theme_minimal() + xlab("Area") + ylab("Price") + stat_smooth(method = "lm", formula = y ~ x, geom = "smooth")
