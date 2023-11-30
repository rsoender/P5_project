library(ggplot2)
library(mapview)
library(sf)
library(mapedit)
library(dplyr)
library(skimr)
library(data.table)
library(RColorBrewer)
library(patchwork)

# Mac
load("/Users/rasmus/Library/CloudStorage/OneDrive-AalborgUniversitet/P5/P5 r/home.Rda")

# Windows
# load("/Users/flaat/OneDrive - Aalborg Universitet/P5/P5 r/home.Rda")

home <- homedata

# Cleaning
home_flat <- subset(home, (SagsStatus == "Solgt" | SagsStatus == "Endelig handel") & DWID_Projektsalg == "Nej" & EjdType == "Ejerlejlighed" & Salgsaar == 2021 & Postnr %in% c(9000, 8000, 5000, c(1050:2450)), select = c(Postnr, Bynavn, Beloeb_EjerUdgift, Pris_EjdVurdering, Pris_Salg, AntalFremvisninger, AntalStatusmoederSaelger, Sag_AnnonceretNettet, Areal_Bolig, Areal_Bolig_Commercial, Adresse_Etage, Areal_GarageCarport, Areal_Kaelder, Ejd_Altan, Ejd_AntalRum, Ejd_Energimaerke, Salgstid, Dist_skole, Alder, Hoejhus, Dist_raadhus, Ejd_AntalSoveVaerelser, Ejd_AntalToiletter, Ejd_AntalPlan, GisX_Wgs84, GisY_Wgs84, row_id))

# Changing NAs to 0 or 1
home_flat <- home_flat %>% mutate(across(c(Areal_Bolig_Commercial, Areal_Kaelder, Ejd_AntalSoveVaerelser, Ejd_AntalToiletter), ~ifelse(is.na(.), 0, .)))
home_flat <- home_flat %>% 
  mutate(Ejd_AntalRum = ifelse(is.na(Ejd_AntalRum), 1, home_flat$Ejd_AntalRum))
home_flat <- home_flat %>% 
  mutate(Ejd_AntalPlan = ifelse(Ejd_AntalPlan == 0, 1, Ejd_AntalPlan))

# Changing Bynavn
home_flat <- home_flat %>%
  mutate(Bynavn = ifelse(Postnr %in% c(1050:2450), "København C", Bynavn))
home_flat <- home_flat %>%
  mutate(Bynavn = ifelse(Postnr == 9000, "Aalborg C", Bynavn))

# Cleaning floor number
home_flat$Adresse_Etage <- ifelse(
  is.na(home_flat$Adresse_Etage) | grepl("(st.|ST|NULL)", home_flat$Adresse_Etage), 
  0, 
  home_flat$Adresse_Etage
)
home_flat$Adresse_Etage <- gsub("(kld.)", -1, gsub("3.", 3, home_flat$Adresse_Etage))

home_flat$Adresse_Etage <- as.integer(home_flat$Adresse_Etage)

# Removing remaining NAs
home_flat <- na.omit(home_flat)

# Making variables categorical
home_flat$Areal_Kaelder <- as.factor(ifelse(home_flat$Areal_Kaelder > 0, 'Yes', 'No')) 

home_flat$Areal_GarageCarport <- as.factor(ifelse(home_flat$Areal_GarageCarport > 0, 'Yes', "No"))

home_flat$Areal_Bolig_Commercial <- as.factor(ifelse(home_flat$Areal_Bolig_Commercial > 0, 'Yes', "No"))

home_flat$Ejd_AntalPlan <- as.factor(ifelse(home_flat$Ejd_AntalPlan > 1, 'Multiple', "Single"))

# Refactor Energimaerke and AnnonceretNettet
home_flat$Ejd_Energimaerke <- factor(home_flat$Ejd_Energimaerke)
home_flat$Ejd_Energimaerke <- relevel(home_flat$Ejd_Energimaerke, ref = "Unknown")
home_flat$Sag_AnnonceretNettet <- factor(home_flat$Sag_AnnonceretNettet)
home_flat$Sag_AnnonceretNettet <- relevel(home_flat$Sag_AnnonceretNettet, ref = "Nej")
home_flat$Ejd_Altan <- factor(home_flat$Ejd_Altan)
home_flat$Ejd_Altan <- relevel(home_flat$Ejd_Altan, ref = "Nej")
home_flat$Ejd_AntalPlan <- factor(home_flat$Ejd_AntalPlan)
home_flat$Ejd_AntalPlan <- relevel(home_flat$Ejd_AntalPlan, ref = "Single")

# Model
set.seed(123)

rows_to_delete <- round(0.20 * nrow(home_flat))
indices_to_delete <- sample(seq_len(nrow(home_flat)), size = rows_to_delete, replace = FALSE)

home_flat_80 <- home_flat[-indices_to_delete, ]

null_model <- lm(Pris_Salg ~ 1, data = home_flat_80)
null_model_log <- lm(log(Pris_Salg) ~ 1, data = home_flat_80)

# QQ-plots null-model normal and log
ggplot(data.frame(sample = rstandard(null_model)/1000000), aes(sample = rstandard(null_model))) +
  stat_qq(color = "#3399cc") +
  stat_qq_line(linetype = "dotted") +
  theme_minimal() +
  labs(x ="Theoretical quantiles",
       y = "Standardised residuals in mill DKK")

ggplot(data.frame(sample = rstandard(null_model_log))/1000000, aes(sample = rstandard(null_model_log))) +
  stat_qq(color = "#3399cc") +
  stat_qq_line(linetype = "dotted") +
  theme_minimal() +
  labs(x ="Theoretical quantiles",
       y = "Standardised residuals in mill DKK")


model <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger + AntalStatusmoederSaelger + Areal_Bolig + Areal_Bolig_Commercial + Areal_GarageCarport + Areal_Kaelder + Beloeb_EjerUdgift + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalPlan + Ejd_AntalRum + Ejd_AntalSoveVaerelser + Ejd_AntalToiletter + Ejd_Energimaerke + Pris_EjdVurdering + Sag_AnnonceretNettet + Salgstid, data = home_flat_80)

summary(model)

# Removing outliers
outliers <- c(763, 621, 447, 437, 832)
colors <- rep("#3399cc", length(residuals(model)))
colors[outliers] <- "#ff6347"  
plot(model, which = 2, col = colors, pch = 20, id.n = 5, caption = NULL, cex.caption = 0.5)
par(col = "gray", lty = 1)
grid()

home_flat_80 <- home_flat_80[-outliers, ]

model_2 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger + AntalStatusmoederSaelger + Areal_Bolig + Areal_Bolig_Commercial + Areal_GarageCarport + Areal_Kaelder + Beloeb_EjerUdgift + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalPlan + Ejd_AntalRum + Ejd_AntalSoveVaerelser + Ejd_AntalToiletter + Ejd_Energimaerke + Pris_EjdVurdering + Sag_AnnonceretNettet + Salgstid, data = home_flat_80)

colors <- rep("#3399cc", length(residuals(model)))
plot(model_2, which = 5, col = colors, pch = 20, id.n = 0, caption = NULL, cex.caption = 0.5)
par(col = "gray", lty = 1)
grid()

home_flat_80 <- home_flat_80[-785, ]

model_3 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger + AntalStatusmoederSaelger + Areal_Bolig + Areal_Bolig_Commercial + Areal_GarageCarport + Areal_Kaelder + Beloeb_EjerUdgift + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalPlan + Ejd_AntalRum + Ejd_AntalSoveVaerelser + Ejd_AntalToiletter + Ejd_Energimaerke + Pris_EjdVurdering + Sag_AnnonceretNettet + Salgstid, data = home_flat_80)

plot(model_3, which = 5, col = colors, pch = 20, id.n = 0, caption = NULL, cex.caption = 0.5)
par(col = "gray", lty = 1)
grid()

# Cook's distance cut-off
home_flat_80_cutoff <- home_flat_80[cooks.distance(model_3) <= 4/(nrow(home_flat_80)-length(model_3$coefficients + 1)), ]

model_4 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger + AntalStatusmoederSaelger + Areal_Bolig + Areal_Bolig_Commercial + Areal_GarageCarport + Areal_Kaelder + Beloeb_EjerUdgift + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalPlan + Ejd_AntalRum + Ejd_AntalSoveVaerelser + Ejd_AntalToiletter + Ejd_Energimaerke + Pris_EjdVurdering + Sag_AnnonceretNettet + Salgstid, data = home_flat_80_cutoff)

plot(model_4, which = 5, col = colors, pch = 20, id.n = 0, caption = NULL, cex.caption = 0.5)
par(col = "gray", lty = 1)
grid()

summary(model_3)
summary(model_4)

ggplot(model_3, aes(x=model_3$fitted.values, y=rstandard(model_3), col = factor(ifelse(abs(rstandard(model_3)) > 5, 2, 1)))) +
  geom_point(color="#3399cc") +
  xlab("Fitted values") +
  ylab("Standardised residuals") +
  theme_minimal() +
  guides(col = guide_legend(title = NULL)) + 
  geom_smooth(method = "loess", size = 0.5, se = FALSE, show.legend = FALSE)

# Squaring terms
summary(model_3)

model_squaring <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger + AntalStatusmoederSaelger + I(log(Areal_Bolig)) + Areal_Bolig_Commercial + Areal_GarageCarport + Areal_Kaelder + Beloeb_EjerUdgift + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalPlan + Ejd_AntalRum + Ejd_AntalSoveVaerelser + Ejd_AntalToiletter + Ejd_Energimaerke + Pris_EjdVurdering + Sag_AnnonceretNettet + Salgstid, data = home_flat_80)

ggplot(model_3, aes(x=home_flat_80$Areal_Bolig, y=rstandard(model_3))) +
  geom_point(color="#3399cc") +
  xlab("Area of flat") +
  ylab("Standardised residuals") +
  guides(col = guide_legend(title = NULL)) +
  theme_minimal()

ggplot(model_squaring, aes(x=home_flat_80$Areal_Bolig, y=rstandard(model_squaring))) +
  geom_point(color="#3399cc") +
  xlab("Area of flat") +
  ylab("Standardised residuals (squared term)") +
  guides(col = guide_legend(title = NULL)) +
  theme_minimal()

summary(model_squaring)

model_squaring <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger + AntalStatusmoederSaelger + Areal_Bolig + Areal_Bolig_Commercial + Areal_GarageCarport + Areal_Kaelder + I(Beloeb_EjerUdgift^2) + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalPlan + Ejd_AntalRum + Ejd_AntalSoveVaerelser + Ejd_AntalToiletter + Ejd_Energimaerke + Pris_EjdVurdering + Sag_AnnonceretNettet + Salgstid, data = home_flat_80)

ggplot(model_3, aes(x=home_flat_80$Beloeb_EjerUdgift, y=rstandard(model_3))) +
  geom_point(color="#3399cc") +
  xlab("Ownership tax") +
  ylab("Standardised residuals") +
  guides(col = guide_legend(title = NULL)) +
  theme_minimal()

summary(model_squaring)

ggplot(model_squaring, aes(x=home_flat_80$Areal_Bolig, y=rstandard(model_squaring))) +
  geom_point(color="#3399cc") +
  xlab("Ownership tax") +
  ylab("Standardised residuals (squared term)") +
  guides(col = guide_legend(title = NULL)) +
  theme_minimal()

model_squaring <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger + AntalStatusmoederSaelger + Areal_Bolig + Areal_Bolig_Commercial + Areal_GarageCarport + Areal_Kaelder + Beloeb_EjerUdgift + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalPlan + Ejd_AntalRum + Ejd_AntalSoveVaerelser + Ejd_AntalToiletter + Ejd_Energimaerke + I(Pris_EjdVurdering^(1/2)) + Sag_AnnonceretNettet + Salgstid, data = home_flat_80)

ggplot(model_3, aes(x=home_flat_80$Pris_EjdVurdering, y=rstandard(model_3))) +
  geom_point(color="#3399cc") +
  xlab("Property assessment") +
  ylab("Standardised residuals") +
  guides(col = guide_legend(title = NULL)) +
  theme_minimal()

summary(model_squaring)

ggplot(model_squaring, aes(x=home_flat_80$Areal_Bolig, y=rstandard(model_squaring))) +
  geom_point(color="#3399cc") +
  xlab("Property assessment") +
  ylab("Standardised residuals (squared term)") +
  guides(col = guide_legend(title = NULL)) +
  theme_minimal()

model_squaring <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger + AntalStatusmoederSaelger + I(log(Areal_Bolig)) + Areal_Bolig_Commercial + Areal_GarageCarport + Areal_Kaelder + I(Beloeb_EjerUdgift^2) + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalPlan + Ejd_AntalRum + Ejd_AntalSoveVaerelser + Ejd_AntalToiletter + Ejd_Energimaerke + I(Pris_EjdVurdering^(1/2)) + Sag_AnnonceretNettet + Salgstid, data = home_flat_80)

ggplot(model_squaring, aes(x=home_flat_80$Dist_skole, y=rstandard(model_squaring))) +
  geom_point(color="#3399cc") +
  xlab("Property assessment") +
  ylab("Standardised residuals (squared term)") +
  guides(col = guide_legend(title = NULL)) +
  theme_minimal()

Adresse_Etage + Alder + AntalFremvisninger + AntalStatusmoederSaelger + Areal_Bolig + Areal_Bolig_Commercial + Areal_GarageCarport + Areal_Kaelder + Beloeb_EjerUdgift + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalPlan + Ejd_AntalRum + Ejd_AntalSoveVaerelser + Ejd_AntalToiletter + Ejd_Energimaerke + I(Pris_EjdVurdering^(1/2)) + Sag_AnnonceretNettet + Salgstid


model_4 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger + AntalStatusmoederSaelger + Areal_Bolig + Areal_Bolig_Commercial + Areal_GarageCarport + Areal_Kaelder + I(Beloeb_EjerUdgift^2) + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalPlan + Ejd_AntalRum + Ejd_AntalSoveVaerelser + Ejd_AntalToiletter + Ejd_Energimaerke + Pris_EjdVurdering + Sag_AnnonceretNettet + Salgstid, data = home_flat_80)

summary(model_4)

ggplot(model_4, aes(x=model_4$fitted.values, y=rstandard(model_4), col = factor(ifelse(abs(rstandard(model_4)) > 5, 2, 1)))) +
  geom_point(color="#3399cc") +
  xlab("Fitted values") +
  ylab("Standardised residuals") +
  theme_minimal() +
  guides(col = guide_legend(title = NULL)) + 
  geom_smooth(method = "loess", size = 0.5, se = FALSE, show.legend = FALSE)

colors <- rep("#3399cc", length(residuals(model_4)))
plot(model_4, which = 2, col = colors, pch = 20, id.n = 0, caption = NULL, cex.caption = 0.5)
par(col = "gray", lty = 1)
grid()

colors <- rep("#3399cc", length(residuals(model_4)))
plot(model_4, which = 5, col = colors, pch = 20, id.n = 0, caption = NULL, cex.caption = 0.5)
par(col = "gray", lty = 1)
grid()

ggplot(model_4, aes(x=home_flat_80$Areal_Bolig, y=hatvalues(model_4), col = factor(ifelse(abs(rstandard(model_4)) > 5, 2, 1)))) +
  geom_point(color="#3399cc") +
  xlab("Fitted values") +
  ylab("Standardised residuals") +
  theme_minimal() +
  guides(col = guide_legend(title = NULL)) + 
  geom_hline(yintercept=3*mean(hatvalues(model_4)), color = "#ff6347")

# Area definitions
home_flat_aalborg <- home_flat[home_flat$Postnr == 9000,]
home_flat_aarhus <- home_flat[home_flat$Postnr == 8000,]
home_flat_odense <- home_flat[home_flat$Postnr == 5000,]
home_flat_copenhagen <- home_flat[home_flat$Postnr %in% c(1050:2450),]

# Color
custom_palette <- colorRampPalette(rev(brewer.pal(11, "RdYlGn")))

# Map plot
# Aalborg
home_flat_aalborg <- st_as_sf(home_flat_aalborg, coords = c("GisX_Wgs84", "GisY_Wgs84"), crs = 4269)
mapview(home_flat_aalborg, map.types = "OpenStreetMap", zcol = 'Pris_Salg', col.regions=custom_palette, at = c(0.5, 1, 1.25, 1.75, 2.25, 2.5, 3, 3.5, 4)*1000000)

# Aarhus
home_flat_aarhus <- st_as_sf(home_flat_aarhus, coords = c("GisX_Wgs84", "GisY_Wgs84"), crs = 4269)
mapview(home_flat_aarhus, map.types = "OpenStreetMap", zcol = 'Pris_Salg', col.regions=custom_palette, at = c(0.2, 1.5, 2.5, 3, 3.5, 4, 4.5, 5, 7.5)*1000000)

# Odense
home_flat_odense <- st_as_sf(home_flat_odense, coords = c("GisX_Wgs84", "GisY_Wgs84"), crs = 4269)
mapview(home_flat_odense, map.types = "OpenStreetMap", zcol = 'Pris_Salg', col.regions=custom_palette, at = c(0.2, 1.25, 1.5, 2.25, 3, 4, 5, 6, 7)*1000000)

# Copenhagen
home_flat_copenhagen <- st_as_sf(home_flat_copenhagen, coords = c("GisX_Wgs84", "GisY_Wgs84"), crs = 4269)
mapview(home_flat_copenhagen, map.types = "OpenStreetMap", zcol = 'Pris_Salg', col.regions=custom_palette, at = c(0.2, 2, 3, 4, 5, 7.5, 10, 15, 20)*1000000)

# Scatter plot
ggplot(home_aalborg, aes(x = home_aalborg$Areal_Bolig, y = home_aalborg$Pris_Salg)) + geom_point(size =2,shape = ".",alpha = 0.9) + xlim(25, 200) + ylim(0, 6 * 10 ^ 6) + theme_minimal() + xlab("Area") + ylab("Price") + stat_smooth(method = "lm", formula = y ~ x, geom = "smooth")
