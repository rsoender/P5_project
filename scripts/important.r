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
home_flat <- subset(home, (SagsStatus == "Solgt" | SagsStatus == "Endelig handel") & DWID_Projektsalg == "Nej" & EjdType == "Ejerlejlighed" & Postnr %in% c(9000, 8000, 5000, c(1050:2450)) & !is.na(Pris_Salg), select = c(Postnr, Bynavn, Beloeb_EjerUdgift, Pris_EjdVurdering, Pris_Salg, AntalFremvisninger, AntalStatusmoederSaelger, Sag_AnnonceretNettet, Areal_Bolig, Areal_Bolig_Commercial, Adresse_Etage, Areal_GarageCarport, Areal_Kaelder, Ejd_Altan, Ejd_AntalRum, Ejd_Energimaerke, Salgstid, Salgsmaaned, Dist_skole, Alder, Hoejhus, Dist_raadhus, corona))

# Cleaning floor number
home_flat$Adresse_Etage <- ifelse(
  is.na(home_flat$Adresse_Etage) | grepl("(st.|ST|NULL)", home_flat$Adresse_Etage), 
  0, 
  home_flat$Adresse_Etage
)
home_flat$Adresse_Etage <- gsub("(kld.|kl)", -1, gsub("3.", 3, home_flat$Adresse_Etage))
home_flat$Adresse_Etage <- as.integer(home_flat$Adresse_Etage)
table(home_flat$Adresse_Etage)
# Changing NAs to 0
home_flat <- home_flat %>% mutate(across(c(AntalFremvisninger, AntalStatusmoederSaelger, Areal_Bolig_Commercial, Areal_GarageCarport, Areal_Kaelder), ~ifelse(is.na(.), 0, .)))

# Changing Bynavn
home_flat <- home_flat %>%
  mutate(Bynavn = ifelse(Postnr %in% c(1050:2450), "København C", Bynavn))

# Cleaning Areal_Bolig_Commercial
home_flat <- home_flat[home_flat$Areal_Bolig_Commercial >= 0,]

# Cleaning energimærker
home_flat$Ejd_Energimaerke <- gsub("A[0-9]+", "A", home_flat$Ejd_Energimaerke)
home_flat <- home_flat[home_flat$Ejd_Energimaerke != "Unknown",]

# Removing remaining NAs 
home_flat <- na.omit(home_flat)

# Model 1
model <- lm(Pris_Salg ~ Bynavn + Beloeb_EjerUdgift + Pris_EjdVurdering + AntalFremvisninger + AntalStatusmoederSaelger + Sag_AnnonceretNettet + Areal_Bolig + Areal_Bolig_Commercial + Adresse_Etage + Areal_GarageCarport + Areal_Kaelder + Ejd_Altan + Ejd_AntalRum + Ejd_Energimaerke + Salgstid + Salgsmaaned + Dist_skole + Alder + Hoejhus + Dist_raadhus + corona, data = home_flat)

summary(model)
plot(model, which = 1)

res <- residuals(model)
qqnorm(res)
qqline(res, col = "darkorange", lwd = 2)

# Model 2 - log
model_log <- lm(log(Pris_Salg) ~ Bynavn + Beloeb_EjerUdgift + Pris_EjdVurdering + AntalFremvisninger + AntalStatusmoederSaelger + Sag_AnnonceretNettet + Areal_Bolig + Areal_Bolig_Commercial + Adresse_Etage + Areal_GarageCarport + Areal_Kaelder + Ejd_Altan + Ejd_AntalRum + Ejd_Energimaerke + Salgstid + Salgsmaaned + Dist_skole + Alder + Hoejhus + Dist_raadhus + corona, data = home_flat)

summary(model_log)
plot(model_log, which = 1)

res_log <- residuals(model_log)
qqnorm(res_log)
qqline(res_log, col = "darkorange", lwd = 2)

# Area definitions
home_aalborg <- home[home$Postnr == 9000,]
home_aarhus <- home[home$Postnr == 8000,]
home_odense <- home[home$Postnr == 5000,]
home_copenhagen <- home[home$Postnr %in% c(1050:2450),] %>% mutate(Bynavn = "København C")# 1473 or 2450

# Map plot
# Aalborg
home_aalborg <- st_as_sf(home_aalborg, coords = c("GisX_Wgs84", "GisY_Wgs84"), crs = 4269)
mapview(home_aalborg, map.types = "OpenStreetMap", zcol = "Pris_Salg")

# Aarhus
home_aarhus <- st_as_sf(home_aarhus, coords = c("GisX_Wgs84", "GisY_Wgs84"), crs = 4269)
mapview(home_aarhus, map.types = "OpenStreetMap")

# Odense
home_odense <- st_as_sf(home_odense, coords = c("GisX_Wgs84", "GisY_Wgs84"), crs = 4269)
mapview(home_odense, map.types = "OpenStreetMap")

# Copenhagen
home_copenhagen <- st_as_sf(home_copenhagen, coords = c("GisX_Wgs84", "GisY_Wgs84"), crs = 4269)
mapview(home_copenhagen, map.types = "OpenStreetMap")
