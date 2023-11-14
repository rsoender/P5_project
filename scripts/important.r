library(ggplot2)
library(mapview)
library(sf)
library(mapedit)
library(dplyr)
library(skimr)
library(data.table)
library(RColorBrewer)

# Mac
load("/Users/rasmus/Library/CloudStorage/OneDrive-AalborgUniversitet/P5/P5 r/home.Rda")

# Windows
# load("/Users/flaat/OneDrive - Aalborg Universitet/P5/P5 r/home.Rda")

home <- homedata

# Cleaning
home_flat <- subset(home, (SagsStatus == "Solgt" | SagsStatus == "Endelig handel") & DWID_Projektsalg == "Nej" & EjdType == "Ejerlejlighed" & Postnr %in% c(9000, 8000, 5000, c(1050:2450)) & !is.na(Pris_Salg), select = c(Postnr, Bynavn, Beloeb_EjerUdgift, Pris_EjdVurdering, Pris_Salg, AntalFremvisninger, AntalStatusmoederSaelger, Sag_AnnonceretNettet, Areal_Bolig, Areal_Bolig_Commercial, Adresse_Etage, Areal_GarageCarport, Areal_Kaelder, Ejd_Altan, Ejd_AntalRum, Ejd_Energimaerke, Salgstid, Salgsmaaned, Dist_skole, Alder, Hoejhus, Dist_raadhus, corona, row_id))

# Cleaning floor number
home_flat$Adresse_Etage <- ifelse(
  is.na(home_flat$Adresse_Etage) | grepl("(st.|ST|NULL)", home_flat$Adresse_Etage), 
  0, 
  home_flat$Adresse_Etage
)
home_flat$Adresse_Etage <- gsub("(kld.|kl)", -1, gsub("3.", 3, home_flat$Adresse_Etage))
home_flat$Adresse_Etage <- as.integer(home_flat$Adresse_Etage)

# Changing NAs to 0
home_flat <- home_flat %>% mutate(across(c(AntalFremvisninger, AntalStatusmoederSaelger, Areal_Bolig_Commercial, Areal_GarageCarport, Areal_Kaelder), ~ifelse(is.na(.), 0, .)))

# Changing Bynavn
home_flat <- home_flat %>%
  mutate(Bynavn = ifelse(Postnr %in% c(1050:2450), "København C", Bynavn))
home_flat <- home_flat %>%
  mutate(Bynavn = ifelse(Postnr == 9000, "Aalborg C", Bynavn))

# Cleaning Areal_Bolig_Commercial
home_flat <- home_flat[home_flat$Areal_Bolig_Commercial >= 0,]

# Cleaning energimærker
home_flat$Ejd_Energimaerke <- gsub("A[0-9]+", "A", home_flat$Ejd_Energimaerke)
home_flat <- home_flat[home_flat$Ejd_Energimaerke != "Unknown",]

# Removing observations outside city centres. 
row_ids_to_remove <- c(
  #aalborg
  29101,  42526,  60595,  62214,  88833,  98346, 112283, 136672, 140368, 145849,  
  #aarhus
  43992,
  #odense
  35603,  54190,  60151,  63789,  72947,  73759,  77612,  77660,  79946,  83737,
  83882,  87886,  88161,  89676,  92048,  95461,  95945, 100556, 101427, 102012,
  113594, 119837, 125736, 149713, 151153, 102379,
  #copenhagen
  20476, 21213, 22245, 24311, 24873, 25481, 27089, 27207, 29490, 32501, 33024, 
  42514, 43387, 48029, 48034, 50043, 50177, 52059, 52477, 53541, 54215, 60067, 
  60415, 60633, 64257, 64599, 65543, 66285, 66293, 68707, 70082, 70377, 71036, 
  72160, 73291, 76479, 77222, 77333, 80765, 82461, 86233, 86653, 89102, 91441, 
  96019, 98155, 98158, 99120, 100446, 101871, 103521, 105354, 107717, 107843, 
  110954, 111164, 112607, 113188, 115654, 116002, 116952, 117022, 118743, 119578,
  120462, 121418, 122748, 123090, 123239, 124242, 127884, 128364, 128503, 130018, 
  131369, 132801, 133962, 134400, 134923, 135117, 135719, 136854, 137235, 137792, 
  138550, 139718, 139971, 140161, 142325, 143059, 143247, 144503, 144718, 146115, 
  147180, 148315, 148348, 148926, 149426, 149697, 150131, 150318, 150337, 150634, 
  150673, 150781, 151626, 151858, 151869, 152055, 152154, 152418, 153552, 154617, 
  154818, 155350, 156601, 156726, 157337, 157967, 158407
)
rows_to_remove <- which(home_flat$row_id %in% row_ids_to_remove)
home_flat <- home_flat[-rows_to_remove, ]

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
nrow(home_flat[home_flat$Postnr %in% c(5000),])

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
