library(ggplot2)
library(mapview)
library(sf)
library(mapedit)
library(dplyr)
library(skimr)
library(data.table)
library(RColorBrewer)
library(patchwork)
library(tidyr)
library(lmtest)
library(skedastic)
library(tsoutliers)
library(Metrics)
library(moments)

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

# Scatter plot sqm price and area
ggplot(home_flat, aes(x=home_flat$Bynavn, y=home_flat$Pris_Salg/1000/home_flat$Areal_Bolig)) +
  geom_point(color="#3399cc") +
  xlab("City name") +
  ylab("Square meter price (in thousands DKK)") +
  theme_minimal() +
  guides(col = guide_legend(title = NULL))

# Model
set.seed(123)

rows_to_delete <- round(0.20 * nrow(home_flat))
indices_to_delete <- sample(seq_len(nrow(home_flat)), size = rows_to_delete, replace = FALSE)

home_flat_80 <- home_flat[-indices_to_delete, ]
home_flat_20 <- home_flat[indices_to_delete, ]

null_model <- lm(Pris_Salg ~ 1, data = home_flat_80)
null_model_log <- lm(log(Pris_Salg) ~ 1, data = home_flat_80)

# QQ-plots null-model normal and log
ggplot(data.frame(sample = rstandard(null_model)/1000000), aes(sample = rstandard(null_model))) +
  stat_qq(color = "#3399cc") +
  stat_qq_line(linetype = "dotted") +
  theme_minimal() +
  labs(x ="Theoretical quantiles",
       y = "Standardised residuals (in million DKK)")

ggplot(data.frame(sample = rstandard(null_model_log))/1000000, aes(sample = rstandard(null_model_log))) +
  stat_qq(color = "#3399cc") +
  stat_qq_line(linetype = "dotted") +
  theme_minimal() +
  labs(x ="Theoretical quantiles",
       y = "Standardised residuals (in million DKK)")


model <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger + AntalStatusmoederSaelger + Areal_Bolig + Areal_Bolig_Commercial + Areal_GarageCarport + Areal_Kaelder + Beloeb_EjerUdgift + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalPlan + Ejd_AntalRum + Ejd_AntalSoveVaerelser + Ejd_AntalToiletter + Ejd_Energimaerke + Pris_EjdVurdering + Sag_AnnonceretNettet + Salgstid, data = home_flat_80)

summary(model)

# Removing outliers

outliers <- c(683, 558, 406, 396, 746)
colors <- rep("#3399cc", length(residuals(model)))
colors[outliers] <- "#ff6347"  
plot(model, which = 2, col = colors, pch = 20, id.n = 0, caption = NULL, cex.caption = 0.5)
par(col = "gray", lty = 1)
grid()

home_flat_80 <- home_flat_80[-outliers, ]

model_2 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger + AntalStatusmoederSaelger + Areal_Bolig + Areal_Bolig_Commercial + Areal_GarageCarport + Areal_Kaelder + Beloeb_EjerUdgift + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalPlan + Ejd_AntalRum + Ejd_AntalSoveVaerelser + Ejd_AntalToiletter + Ejd_Energimaerke + Pris_EjdVurdering + Sag_AnnonceretNettet + Salgstid, data = home_flat_80)

colors <- rep("#3399cc", length(residuals(model)))
plot(model_2, which = 5, col = colors, pch = 20, id.n = 0, caption = NULL, cex.caption = 0.5)
par(col = "gray", lty = 1)
grid()

home_flat_80 <- home_flat_80[-701, ]

model_3 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger + AntalStatusmoederSaelger + Areal_Bolig + Areal_Bolig_Commercial + Areal_GarageCarport + Areal_Kaelder + Beloeb_EjerUdgift + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalPlan + Ejd_AntalRum + Ejd_AntalSoveVaerelser + Ejd_AntalToiletter + Ejd_Energimaerke + Pris_EjdVurdering + Sag_AnnonceretNettet + Salgstid, data = home_flat_80)

plot(model_3, which = 5, col = colors, pch = 20, id.n = 0, caption = NULL, cex.caption = 0.5)
par(col = "gray", lty = 1)
grid()

summary(model_3)

# Cook's distance cut-off
home_flat_80_cutoff <- home_flat_80[cooks.distance(model_3) <= 4/(nrow(home_flat_80)-length(model_3$coefficients + 1)), ]

model_4 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger + AntalStatusmoederSaelger + Areal_Bolig + Areal_Bolig_Commercial + Areal_GarageCarport + Beloeb_EjerUdgift + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalPlan + Ejd_AntalRum + Ejd_AntalSoveVaerelser + Ejd_AntalToiletter + Ejd_Energimaerke + Pris_EjdVurdering + Sag_AnnonceretNettet + Salgstid, data = home_flat_80_cutoff)

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

# Changing terms
summary(model_3)

model_transforming <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger + AntalStatusmoederSaelger + I(log(Areal_Bolig)) + Areal_Bolig_Commercial + Areal_GarageCarport + Areal_Kaelder + Beloeb_EjerUdgift + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalPlan + Ejd_AntalRum + Ejd_AntalSoveVaerelser + Ejd_AntalToiletter + Ejd_Energimaerke + Pris_EjdVurdering + Sag_AnnonceretNettet + Salgstid, data = home_flat_80)

ggplot(model_3, aes(x=home_flat_80$Areal_Bolig, y=rstandard(model_3))) +
  geom_point(color="#3399cc") +
  xlab("Area of flat") +
  ylab("Standardised residuals") +
  guides(col = guide_legend(title = NULL)) +
  theme_minimal()

ggplot(model_transforming, aes(x=home_flat_80$Areal_Bolig, y=rstandard(model_transforming))) +
  geom_point(color="#3399cc") +
  xlab("Area of flat") +
  ylab("Standardised residuals (squared term)") +
  guides(col = guide_legend(title = NULL)) +
  theme_minimal()

summary(model_transforming)

model_transforming <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger + AntalStatusmoederSaelger + Areal_Bolig + Areal_Bolig_Commercial + Areal_GarageCarport + Areal_Kaelder + Beloeb_EjerUdgift + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalPlan + Ejd_AntalRum + Ejd_AntalSoveVaerelser + Ejd_AntalToiletter + Ejd_Energimaerke + I(Pris_EjdVurdering^(1/2)) + Sag_AnnonceretNettet + Salgstid, data = home_flat_80)

ggplot(model_3, aes(x=home_flat_80$Pris_EjdVurdering, y=rstandard(model_3))) +
  geom_point(color="#3399cc") +
  xlab("Property assessment") +
  ylab("Standardised residuals") +
  guides(col = guide_legend(title = NULL)) +
  theme_minimal()

ggplot(model_transforming, aes(x=home_flat_80$Pris_EjdVurdering, y=rstandard(model_transforming))) +
  geom_point(color="#3399cc") +
  xlab("Property assessment") +
  ylab("Standardised residuals (squared term)") +
  guides(col = guide_legend(title = NULL)) +
  theme_minimal()

summary(model_transforming)

model_4 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger + AntalStatusmoederSaelger + I(log(Areal_Bolig)) + Areal_Bolig_Commercial + Areal_GarageCarport + Areal_Kaelder + Beloeb_EjerUdgift + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalPlan + Ejd_AntalRum + Ejd_AntalSoveVaerelser + Ejd_AntalToiletter + Ejd_Energimaerke + I(Pris_EjdVurdering^(1/2)) + Sag_AnnonceretNettet + Salgstid, data = home_flat_80)

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

plot(model_4, which = 1, col = colors, pch = 20, id.n = 0, caption = NULL, cex.caption = 0.5)
par(col = "gray", lty = 1)
grid()

ggplot(model_4, aes(x=model_4$fitted.values, y=rstandard(model_4), col = factor(ifelse(abs(rstandard(model_4)) > 5, 2, 1)))) +
  geom_point(color="#3399cc") +
  xlab("Fitted values") +
  ylab("Standardised residuals") +
  theme_minimal() +
  guides(col = guide_legend(title = NULL)) + 
  geom_smooth(method = "loess", size = 0.5, se = FALSE, show.legend = FALSE)

# Model reduction
summary(model_4)

home_flat_80_model_reduction <- home_flat_80

cat(rownames(summary(model_4)$coefficients)[which.max(summary(model_4)$coefficients[, "Pr(>|t|)"])]) # Ejd_EnergimaerkeB

levels(home_flat_80_model_reduction$Ejd_Energimaerke)[levels(home_flat_80_model_reduction$Ejd_Energimaerke)=="B"]<-"Unknown"

model_5 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger + AntalStatusmoederSaelger + I(log(Areal_Bolig)) + Areal_Bolig_Commercial + Areal_GarageCarport + Areal_Kaelder + Beloeb_EjerUdgift + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalPlan + Ejd_AntalRum + Ejd_AntalSoveVaerelser + Ejd_AntalToiletter + Ejd_Energimaerke + I(Pris_EjdVurdering^(1/2)) + Sag_AnnonceretNettet + Salgstid, data = home_flat_80_model_reduction)

cat(rownames(summary(model_5)$coefficients)[which.max(summary(model_5)$coefficients[, "Pr(>|t|)"])]) # Ejd_EnergimaerkeF

levels(home_flat_80_model_reduction$Ejd_Energimaerke)[levels(home_flat_80_model_reduction$Ejd_Energimaerke)=="F"]<-"Unknown"

model_5 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger + AntalStatusmoederSaelger + I(log(Areal_Bolig)) + Areal_Bolig_Commercial + Areal_GarageCarport + Areal_Kaelder + Beloeb_EjerUdgift + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalPlan + Ejd_AntalRum + Ejd_AntalSoveVaerelser + Ejd_AntalToiletter + Ejd_Energimaerke + I(Pris_EjdVurdering^(1/2)) + Sag_AnnonceretNettet + Salgstid, data = home_flat_80_model_reduction)

cat(rownames(summary(model_5)$coefficients)[which.max(summary(model_5)$coefficients[, "Pr(>|t|)"])]) # Ejd_EnergimaerkeA2010

levels(home_flat_80_model_reduction$Ejd_Energimaerke)[levels(home_flat_80_model_reduction$Ejd_Energimaerke)=="A2010"]<-"Unknown"

model_5 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger + AntalStatusmoederSaelger + I(log(Areal_Bolig)) + Areal_Bolig_Commercial + Areal_GarageCarport + Areal_Kaelder + Beloeb_EjerUdgift + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalPlan + Ejd_AntalRum + Ejd_AntalSoveVaerelser + Ejd_AntalToiletter + Ejd_Energimaerke + I(Pris_EjdVurdering^(1/2)) + Sag_AnnonceretNettet + Salgstid, data = home_flat_80_model_reduction)

cat(rownames(summary(model_5)$coefficients)[which.max(summary(model_5)$coefficients[, "Pr(>|t|)"])]) # Areal_Bolig_CommercialYes

model_5 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger + AntalStatusmoederSaelger + I(log(Areal_Bolig)) + Areal_GarageCarport + Areal_Kaelder + Beloeb_EjerUdgift + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalPlan + Ejd_AntalRum + Ejd_AntalSoveVaerelser + Ejd_AntalToiletter + Ejd_Energimaerke + I(Pris_EjdVurdering^(1/2)) + Sag_AnnonceretNettet + Salgstid, data = home_flat_80_model_reduction)

cat(rownames(summary(model_5)$coefficients)[which.max(summary(model_5)$coefficients[, "Pr(>|t|)"])]) # Ejd_AntalPlanMultiple

model_5 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger + AntalStatusmoederSaelger + I(log(Areal_Bolig)) + Areal_GarageCarport + Areal_Kaelder + Beloeb_EjerUdgift + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalRum + Ejd_AntalSoveVaerelser + Ejd_AntalToiletter + Ejd_Energimaerke + I(Pris_EjdVurdering^(1/2)) + Sag_AnnonceretNettet + Salgstid, data = home_flat_80_model_reduction)

cat(rownames(summary(model_5)$coefficients)[which.max(summary(model_5)$coefficients[, "Pr(>|t|)"])]) # Areal_KaelderYes

model_5 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger + AntalStatusmoederSaelger + I(log(Areal_Bolig)) + Areal_GarageCarport + Beloeb_EjerUdgift + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalRum + Ejd_AntalSoveVaerelser + Ejd_AntalToiletter + Ejd_Energimaerke + I(Pris_EjdVurdering^(1/2)) + Sag_AnnonceretNettet + Salgstid, data = home_flat_80_model_reduction)

cat(rownames(summary(model_5)$coefficients)[which.max(summary(model_5)$coefficients[, "Pr(>|t|)"])]) # Ejd_EnergimaerkeE

levels(home_flat_80_model_reduction$Ejd_Energimaerke)[levels(home_flat_80_model_reduction$Ejd_Energimaerke)=="E"]<-"Unknown"
 
model_5 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger + AntalStatusmoederSaelger + I(log(Areal_Bolig)) + Areal_GarageCarport + Beloeb_EjerUdgift + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalRum + Ejd_AntalSoveVaerelser + Ejd_AntalToiletter + Ejd_Energimaerke + I(Pris_EjdVurdering^(1/2)) + Sag_AnnonceretNettet + Salgstid, data = home_flat_80_model_reduction)

cat(rownames(summary(model_5)$coefficients)[which.max(summary(model_5)$coefficients[, "Pr(>|t|)"])]) # Ejd_EnergimaerkeG

levels(home_flat_80_model_reduction$Ejd_Energimaerke)[levels(home_flat_80_model_reduction$Ejd_Energimaerke)=="G"]<-"Unknown"

model_5 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger + AntalStatusmoederSaelger + I(log(Areal_Bolig)) + Areal_GarageCarport + Beloeb_EjerUdgift + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalRum + Ejd_AntalSoveVaerelser + Ejd_AntalToiletter + Ejd_Energimaerke + I(Pris_EjdVurdering^(1/2)) + Sag_AnnonceretNettet + Salgstid, data = home_flat_80_model_reduction)

cat(rownames(summary(model_5)$coefficients)[which.max(summary(model_5)$coefficients[, "Pr(>|t|)"])]) # AntalFremvisninger

model_5 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalStatusmoederSaelger + I(log(Areal_Bolig)) + Areal_GarageCarport + Beloeb_EjerUdgift + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalRum + Ejd_AntalSoveVaerelser + Ejd_AntalToiletter + Ejd_Energimaerke + I(Pris_EjdVurdering^(1/2)) + Sag_AnnonceretNettet + Salgstid, data = home_flat_80_model_reduction)

cat(rownames(summary(model_5)$coefficients)[which.max(summary(model_5)$coefficients[, "Pr(>|t|)"])]) # Ejd_EnergimaerkeC

levels(home_flat_80_model_reduction$Ejd_Energimaerke)[levels(home_flat_80_model_reduction$Ejd_Energimaerke)=="C"]<-"Unknown"

model_5 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalStatusmoederSaelger + I(log(Areal_Bolig)) + Areal_GarageCarport + Beloeb_EjerUdgift + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalRum + Ejd_AntalSoveVaerelser + Ejd_AntalToiletter + Ejd_Energimaerke + I(Pris_EjdVurdering^(1/2)) + Sag_AnnonceretNettet + Salgstid, data = home_flat_80_model_reduction)

cat(rownames(summary(model_5)$coefficients)[which.max(summary(model_5)$coefficients[, "Pr(>|t|)"])]) # Ejd_EnergimaerkeD

levels(home_flat_80_model_reduction$Ejd_Energimaerke)[levels(home_flat_80_model_reduction$Ejd_Energimaerke)=="D"]<-"Unknown"

model_5 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalStatusmoederSaelger + I(log(Areal_Bolig)) + Areal_GarageCarport + Beloeb_EjerUdgift + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalRum + Ejd_AntalSoveVaerelser + Ejd_AntalToiletter + Ejd_Energimaerke + I(Pris_EjdVurdering^(1/2)) + Sag_AnnonceretNettet + Salgstid, data = home_flat_80_model_reduction)

cat(rownames(summary(model_5)$coefficients)[which.max(summary(model_5)$coefficients[, "Pr(>|t|)"])]) # Ejd_AntalSoveVaerelser

model_5 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalStatusmoederSaelger + I(log(Areal_Bolig)) + Areal_GarageCarport + Beloeb_EjerUdgift + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalRum + Ejd_AntalToiletter + Ejd_Energimaerke + I(Pris_EjdVurdering^(1/2)) + Sag_AnnonceretNettet + Salgstid, data = home_flat_80_model_reduction)

cat(rownames(summary(model_5)$coefficients)[which.max(summary(model_5)$coefficients[, "Pr(>|t|)"])]) # Ejd_EnergimaerkeA2020

levels(home_flat_80_model_reduction$Ejd_Energimaerke)[levels(home_flat_80_model_reduction$Ejd_Energimaerke)=="A2020"]<-"Unknown"

model_5 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalStatusmoederSaelger + I(log(Areal_Bolig)) + Areal_GarageCarport + Beloeb_EjerUdgift + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalRum + Ejd_AntalToiletter + Ejd_Energimaerke + I(Pris_EjdVurdering^(1/2)) + Sag_AnnonceretNettet + Salgstid, data = home_flat_80_model_reduction)

summary(model_5)

# Model reduction AIC
home_flat_80_model_reduction_AIC <- home_flat_80[home_flat_80$Ejd_Energimaerke != "Unknown", ] %>% 
  pivot_wider(names_from = Ejd_Energimaerke, values_from = Ejd_Energimaerke, values_fn = length, values_fill = 0)

model_6 <- step(lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger + AntalStatusmoederSaelger + I(log(Areal_Bolig)) + Areal_Bolig_Commercial + Areal_GarageCarport + Areal_Kaelder + Beloeb_EjerUdgift + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalPlan + Ejd_AntalRum + Ejd_AntalSoveVaerelser + Ejd_AntalToiletter + I(Pris_EjdVurdering^(1/2)) + Sag_AnnonceretNettet + Salgstid, data = home_flat_80_model_reduction_AIC), direction = "backward")

summary(model_6)

# Model reduction tests
anova(model_5, model_4)
AIC(model_5)
AIC(model_6)

# Bacon test
bptest(model_5)
white(model_5)

model_test <- lm(model_5$residuals^2 ~ Adresse_Etage + Alder + AntalFremvisninger + AntalStatusmoederSaelger + I(log(Areal_Bolig)) + Areal_Bolig_Commercial + Areal_GarageCarport + Areal_Kaelder + Beloeb_EjerUdgift + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalPlan + Ejd_AntalRum + Ejd_AntalSoveVaerelser + Ejd_AntalToiletter + Ejd_Energimaerke + I(Pris_EjdVurdering^(1/2)) + Sag_AnnonceretNettet + Salgstid, data = home_flat_80)

summary(model_test)

nrow(home_flat_80)*0.09788
qchisq(.95, df=18)

# Jarque-Bera test
s_hat <- sum((model_5$residuals-mean(model_5$residuals))^3)/(length(model_5$residuals)*var(model_5$residuals)^(3/2))

k_hat <- sum((model_5$residuals-mean(model_5$residuals))^4)/(length(model_5$residuals)*var(model_5$residuals)^(2))

length(model_5$residuals)*(s_hat^2/6+(k_hat-3)^2/24)
qchisq(.95, df=2)

JarqueBera.test(model_5$residuals)

# Predictions
levels(home_flat_20$Ejd_Energimaerke)[levels(home_flat_20$Ejd_Energimaerke) %in% c('A2010', 'A2020', 'B', 'C', 'D', 'E', 'F', 'G')]<-"Unknown"

pred_int <- predict(model_4, home_flat_20, interval = "prediction")
pred_int_2 <- predict(model_5, home_flat_20, interval = "prediction")

in_pred_int_1 <- sum(log(home_flat_20$Pris_Salg) >= pred_int[,2] & log(home_flat_20$Pris_Salg) <= pred_int[,3])
in_pred_int_2 <- sum(log(home_flat_20$Pris_Salg) >= pred_int_2[,2] & log(home_flat_20$Pris_Salg) <= pred_int_2[,3])
in_pred_int_1 / nrow(home_flat_20) 
in_pred_int_2 / nrow(home_flat_20) 

plot(pred_int_2[,1], log(home_flat_20$Pris_Salg), xlab = "Predicted values of sales price (in log DKK)", ylab = "Sales price (in log DKK)", col = "#3399cc", pch = 20,)
par(col = "gray", lty = 1)
grid()
abline(0, 1, lwd = 2, col = "#ff6347")
conf_int <- predict(model_5, home_flat_20, interval = "confidence")
conf_int <- conf_int[order(conf_int[,1]),]
pred_int_2 <- pred_int_2[order(pred_int_2[,1]),]
lines(pred_int_2[,1], pred_int_2[,2],
      col = "chocolate1", lwd = 2)
lines(pred_int_2[,1], pred_int_2[,3],
      col = "chocolate1", lwd = 2)
lines(pred_int_2[,1], conf_int[,2],
      col = "burlywood4", lwd = 2)
lines(pred_int_2[,1], conf_int[,3],
      col = "burlywood4", lwd = 2)

pred_int_2 <- exp(predict(model_5, home_flat_20, interval = "prediction"))/1000000

plot(pred_int_2[,1], home_flat_20$Pris_Salg/1000000, xlab = "Predicted values of sales price", ylab = "Sales price", col = "#3399cc", pch = 20,)
par(col = "gray", lty = 1)
grid()
abline(0, 1, lwd = 2, col = "#ff6347")
conf_int <- exp(predict(model_5, home_flat_20, interval = "confidence"))/1000000
conf_int <- conf_int[order(conf_int[,1]),]
pred_int_2 <- pred_int_2[order(pred_int_2[,1]),]
lines(pred_int_2[,1], pred_int_2[,2],
      col = "chocolate1", lwd = 2)
lines(pred_int_2[,1], pred_int_2[,3],
      col = "chocolate1", lwd = 2)
lines(pred_int_2[,1], conf_int[,2],
      col = "burlywood4", lwd = 2)
lines(pred_int_2[,1], conf_int[,3],
      col = "burlywood4", lwd = 2)
summary(model_5)

sqrt(sum((pred_int_2[,1]*1000000-home_flat_20$Pris_Salg)^2)/321)

pred_int_2 <- exp(predict(model_5, home_flat_20, interval = "prediction"))
pred_int_2 <- pred_int_2[order(pred_int_2[,1]),]

median(pred_int_2[,1])

pred_int_2[8,2]
pred_int_2[8,3]

# Histogram
diff <- as.data.frame(log(home_flat_20$Pris_Salg)-predict(model_4, home_flat_20, interval = "prediction")[,1])

ggplot(data = diff,  aes(x = diff$`log(home_flat_20$Pris_Salg) - predict(model_4, home_flat_20, interval = "prediction")[, 1]`)) +
  geom_histogram(bins = 40, color = "#3399cc", fill = "#3399cc") +
  theme_minimal() +
  labs(x = "Difference between predicted and actual sales price (in log DKK)", y = "Frequency")

diff <- as.data.frame(home_flat_20$Pris_Salg-exp(predict(model_4, home_flat_20, interval = "prediction")[,1]))/1000000

ggplot(data = diff,  aes(x = diff$`home_flat_20$Pris_Salg - exp(predict(model_4, home_flat_20, interval = "prediction")[, 1])`)) +
  geom_histogram(bins = 40, color = "#3399cc", fill = "#3399cc") +
  theme_minimal() +
  labs(x = "Difference between predicted and actual sales price (in million DKK)", y = "Frequency")

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

