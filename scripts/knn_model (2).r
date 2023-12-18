library(lmtest)
library(tsoutliers)
library(ggplot2)
library(dplyr)
library(skimr)
library(data.table)
library(patchwork)
library(VIM)
library(tidyr)
library(dplyr)

# Windows
load("X:/Credit Risk/MN/Home/Homedata.Rda")

home <- homedata

# Cleaning
home_flat <- subset(home, (SagsStatus == "Solgt" | SagsStatus == "Endelig handel") & DWID_Projektsalg == "Nej" & EjdType == "Ejerlejlighed" & Salgsaar == 2021 & Postnr %in% c(9000, 8000, 5000, c(1050:2450)), select = c(Pris_Salg, Postnr, Bynavn, Beloeb_EjerUdgift, Pris_EjdVurdering, AntalFremvisninger, AntalStatusmoederSaelger, Sag_AnnonceretNettet, Areal_Bolig, Areal_Bolig_Commercial, Adresse_Etage, Areal_GarageCarport, Areal_Kaelder, Ejd_Altan, Ejd_AntalRum, Ejd_Energimaerke, Salgstid, Dist_skole, Alder, Dist_raadhus, Ejd_AntalSoveVaerelser, Ejd_AntalToiletter, Ejd_AntalPlan, row_id))

# Changing NAs to 0 or 1
home_flat <- home_flat %>% mutate(across(c(Areal_Bolig_Commercial, Areal_Kaelder, Ejd_AntalSoveVaerelser, Ejd_AntalToiletter), ~ifelse(is.na(.), 0, .)))
home_flat <- home_flat %>% 
  mutate(Ejd_AntalRum = ifelse(is.na(Ejd_AntalRum), 1, home_flat$Ejd_AntalRum))

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
home_flat$Adresse_Etage <- gsub("(kld.|kl)", -1, gsub("3.", 3, home_flat$Adresse_Etage))

home_flat$Adresse_Etage <- as.integer(home_flat$Adresse_Etage)

# Cleaning energimærker
home_flat$Ejd_Energimaerke <- factor(home_flat$Ejd_Energimaerke)
home_flat$Ejd_Energimaerke <- relevel(home_flat$Ejd_Energimaerke, ref = "Unknown")
home_flat$Ejd_Energimaerke <- as.factor(replace(home_flat$Ejd_Energimaerke, home_flat$Ejd_Energimaerke == "Unknown", NA))
home_flat$Ejd_Energimaerke <- as.factor(replace(home_flat$Ejd_Energimaerke, home_flat$Ejd_Energimaerke == "A2", NA))


n = nrow(home_flat)

k =  round(sqrt(n))

# Convert data.table to data.frame
home_flat_df <- as.data.frame(home_flat)

columns_with_missing <- colnames(home_flat)[apply(home_flat, 2, anyNA)]

# Impute missing values using KNN
home_flat_imputed <- kNN(home_flat_df, k = k, weightDist = FALSE, metric = 'gower')

# Replace the imputed values back into the original dataset
for (col in columns_with_missing) {
  home_flat[[col]] <- home_flat_imputed[[col]]
}


# Save the imputed data
write.csv(home_flat_imputed, file = "imputed_data.csv", row.names = FALSE)

# Convert back to data.table if needed
home_flat <- as.data.table(home_flat)

# Making variables categorical
home_flat$Areal_Kaelder <- as.factor(ifelse(home_flat$Areal_Kaelder > 0, 'Yes', 'No')) 

home_flat$Areal_GarageCarport <- as.factor(ifelse(home_flat$Areal_GarageCarport > 0, 'Yes', "No"))

home_flat$Areal_Bolig_Commercial <- as.factor(ifelse(home_flat$Areal_Bolig_Commercial > 0, 'Yes', "No"))

home_flat$Ejd_AntalPlan <- as.factor(ifelse(home_flat$Ejd_AntalPlan > 1, 'Multiple', "Single"))

# Refactor Energimaerke and AnnonceretNettet
home_flat$Ejd_Energimaerke <- factor(home_flat$Ejd_Energimaerke)
home_flat$Ejd_Energimaerke <- relevel(home_flat$Ejd_Energimaerke, ref = "G")
home_flat$Sag_AnnonceretNettet <- factor(home_flat$Sag_AnnonceretNettet)
home_flat$Sag_AnnonceretNettet <- relevel(home_flat$Sag_AnnonceretNettet, ref = "Nej")
home_flat$Ejd_Altan <- factor(home_flat$Ejd_Altan)
home_flat$Ejd_Altan <- relevel(home_flat$Ejd_Altan, ref = "Nej")
home_flat$Ejd_AntalPlan <- factor(home_flat$Ejd_AntalPlan)
home_flat$Ejd_AntalPlan <- relevel(home_flat$Ejd_AntalPlan, ref = "Single")

set.seed(123)

# Specify the row IDs
row_ids_to_find <- c(144238, 142066, 139775, 139635, 145056)

# Find the row indices in the original dataset
row_indices <- match(row_ids_to_find, home_flat$row_id)

home_flat <- home_flat[-row_indices, ]

rows_to_delete <- round(0.20 * nrow(home_flat))
indices_to_delete <- sample(seq_len(nrow(home_flat)), size = rows_to_delete, replace = FALSE)

home_flat_80 <- home_flat[-indices_to_delete, ]
home_flat_20 <- home_flat[indices_to_delete, ]

home_flat <- home_flat_80


#Model 1
model <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger 
                             + AntalStatusmoederSaelger + Areal_Bolig
                             + Areal_Bolig_Commercial 
                             + Areal_GarageCarport + Areal_Kaelder 
                             + Beloeb_EjerUdgift + Bynavn 
                             + Dist_raadhus + Dist_skole 
                             + Ejd_Altan + Ejd_AntalPlan 
                             + Ejd_AntalRum + Ejd_AntalSoveVaerelser 
                             + Ejd_AntalToiletter + Ejd_Energimaerke
                             + Pris_EjdVurdering
                             + Sag_AnnonceretNettet + Salgstid
                             , data = home_flat)

summary(model)

#Model 2
home_flat <- home_flat[-c(285),]
model <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger 
                             + AntalStatusmoederSaelger + Areal_Bolig
                             + Areal_Bolig_Commercial 
                             + Areal_GarageCarport + Areal_Kaelder 
                             + Beloeb_EjerUdgift + Bynavn 
                             + Dist_raadhus + Dist_skole 
                             + Ejd_Altan + Ejd_AntalPlan 
                             + Ejd_AntalRum + Ejd_AntalSoveVaerelser 
                             + Ejd_AntalToiletter + Ejd_Energimaerke
                             + Pris_EjdVurdering
                             + Sag_AnnonceretNettet + Salgstid
                             , data = home_flat)

model_3_data <- home_flat


summary(model)


# Model 3
model <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger 
                             + AntalStatusmoederSaelger + I(log(Areal_Bolig)) 
                             + Areal_Bolig_Commercial 
                             + Areal_GarageCarport + Areal_Kaelder 
                             + Beloeb_EjerUdgift + Bynavn 
                             + Dist_raadhus + Dist_skole 
                             + Ejd_Altan + Ejd_AntalPlan 
                             + Ejd_AntalRum + Ejd_AntalSoveVaerelser 
                             + Ejd_AntalToiletter + Ejd_Energimaerke
                             + I(Pris_EjdVurdering^(1/2))
                             + Sag_AnnonceretNettet + Salgstid
                             , data = model_3_data)


summary(model)



# ggplot(model, aes(x=model$fitted.values, y=rstandard(model), col = factor(ifelse(abs(rstandard(model)) > 5, 2, 1)))) +
#   geom_point(color="#3399cc") +
#   xlab("Fitted values") +
#   ylab("Standardised residuals") +
#   theme_minimal() +
#   guides(col = guide_legend(title = NULL)) + 
#   geom_smooth(method = "loess", size = 0.5, se = FALSE, show.legend = FALSE)


# Model reduction starting from model 3
model_1 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger 
                             + AntalStatusmoederSaelger + I(log(Areal_Bolig)) 
                             + Areal_Bolig_Commercial 
                             + Areal_GarageCarport + Areal_Kaelder 
                             + Beloeb_EjerUdgift + Bynavn 
                             + Dist_raadhus + Dist_skole 
                             + Ejd_Altan + Ejd_AntalPlan 
                             + Ejd_AntalRum + Ejd_AntalSoveVaerelser 
                             + Ejd_AntalToiletter + Ejd_Energimaerke
                             + I(Pris_EjdVurdering^(1/2))
                             + Sag_AnnonceretNettet + Salgstid
                             , data = home_flat)
# Print the summary of the model
summary_model <- summary(model_1)
# Extract the variable name with the highest p-value
variable_with_highest_p <- rownames(summary_model$coefficients)[which.max(summary_model$coefficients[, "Pr(>|t|)"])]
# Print the variable name with the highest p-value and its p-value
cat("Variable with highest p-value:", variable_with_highest_p, "\n")
cat("P-value for", variable_with_highest_p, ":", summary_model$coefficients[variable_with_highest_p, "Pr(>|t|)"], "\n")

levels(home_flat$Ejd_Energimaerke)[levels(home_flat$Ejd_Energimaerke)=="E"]<-"G" # This line removes a category from the categorical variable Energy label, in this case energy label C

model_2 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger 
                             + AntalStatusmoederSaelger + I(log(Areal_Bolig)) 
                             + Areal_Bolig_Commercial 
                             + Areal_GarageCarport + Areal_Kaelder 
                             + Beloeb_EjerUdgift + Bynavn 
                             + Dist_raadhus + Dist_skole 
                             + Ejd_Altan + Ejd_AntalPlan 
                             + Ejd_AntalRum + Ejd_AntalSoveVaerelser 
                             + Ejd_AntalToiletter + Ejd_Energimaerke
                             + I(Pris_EjdVurdering^(1/2))
                             + Sag_AnnonceretNettet + Salgstid
                             , data = home_flat)

summary_model <- summary(model_2)

variable_with_highest_p <- rownames(summary_model$coefficients)[which.max(summary_model$coefficients[, "Pr(>|t|)"])]
cat("Variable with highest p-value:", variable_with_highest_p, "\n")
cat("P-value for", variable_with_highest_p, ":", summary_model$coefficients[variable_with_highest_p, "Pr(>|t|)"], "\n")


model_3 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger 
                             + AntalStatusmoederSaelger + I(log(Areal_Bolig)) 
                             + Areal_Bolig_Commercial 
                             + Areal_GarageCarport  
                             + Beloeb_EjerUdgift + Bynavn 
                             + Dist_raadhus + Dist_skole 
                             + Ejd_Altan + Ejd_AntalPlan 
                             + Ejd_AntalRum + Ejd_AntalSoveVaerelser 
                             + Ejd_AntalToiletter + Ejd_Energimaerke
                             + I(Pris_EjdVurdering^(1/2))
                             + Sag_AnnonceretNettet + Salgstid
                             , data = home_flat)

summary_model <- summary(model_3)

variable_with_highest_p <- rownames(summary_model$coefficients)[which.max(summary_model$coefficients[, "Pr(>|t|)"])]
cat("Variable with highest p-value:", variable_with_highest_p, "\n")
cat("P-value for", variable_with_highest_p, ":", summary_model$coefficients[variable_with_highest_p, "Pr(>|t|)"], "\n")

model_4 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger 
                             + AntalStatusmoederSaelger + I(log(Areal_Bolig))  
                             + Areal_GarageCarport  
                             + Beloeb_EjerUdgift + Bynavn 
                             + Dist_raadhus + Dist_skole 
                             + Ejd_Altan + Ejd_AntalPlan 
                             + Ejd_AntalRum + Ejd_AntalSoveVaerelser 
                             + Ejd_AntalToiletter + Ejd_Energimaerke
                             + I(Pris_EjdVurdering^(1/2))
                             + Sag_AnnonceretNettet + Salgstid
                             , data = home_flat)

summary_model <- summary(model_4)
variable_with_highest_p <- rownames(summary_model$coefficients)[which.max(summary_model$coefficients[, "Pr(>|t|)"])]
cat("Variable with highest p-value:", variable_with_highest_p, "\n")
cat("P-value for", variable_with_highest_p, ":", summary_model$coefficients[variable_with_highest_p, "Pr(>|t|)"], "\n")

model_5 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger 
                             + AntalStatusmoederSaelger + I(log(Areal_Bolig))  
                             + Areal_GarageCarport  
                             + Beloeb_EjerUdgift + Bynavn 
                             + Dist_raadhus + Dist_skole 
                             + Ejd_Altan 
                             + Ejd_AntalRum + Ejd_AntalSoveVaerelser 
                             + Ejd_AntalToiletter + Ejd_Energimaerke
                             + I(Pris_EjdVurdering^(1/2))
                             + Sag_AnnonceretNettet + Salgstid
                             , data = home_flat)

summary_model <- summary(model_5)
variable_with_highest_p <- rownames(summary_model$coefficients)[which.max(summary_model$coefficients[, "Pr(>|t|)"])]
cat("Variable with highest p-value:", variable_with_highest_p, "\n")
cat("P-value for", variable_with_highest_p, ":", summary_model$coefficients[variable_with_highest_p, "Pr(>|t|)"], "\n")

levels(home_flat$Ejd_Energimaerke)[levels(home_flat$Ejd_Energimaerke)=="A2010"]<-"G" # This line removes a category from the categorical variable Energy label, in this case energy label C

model_6 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger 
                             + AntalStatusmoederSaelger + I(log(Areal_Bolig))  
                             + Areal_GarageCarport  
                             + Beloeb_EjerUdgift + Bynavn 
                             + Dist_raadhus + Dist_skole 
                             + Ejd_Altan 
                             + Ejd_AntalRum + Ejd_AntalSoveVaerelser 
                             + Ejd_AntalToiletter + Ejd_Energimaerke
                             + I(Pris_EjdVurdering^(1/2))
                             + Sag_AnnonceretNettet + Salgstid
                             , data = home_flat)

summary_model <- summary(model_6)
variable_with_highest_p <- rownames(summary_model$coefficients)[which.max(summary_model$coefficients[, "Pr(>|t|)"])]
cat("Variable with highest p-value:", variable_with_highest_p, "\n")
cat("P-value for", variable_with_highest_p, ":", summary_model$coefficients[variable_with_highest_p, "Pr(>|t|)"], "\n")

model_7 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger 
                             + AntalStatusmoederSaelger + I(log(Areal_Bolig))  
                             + Areal_GarageCarport  
                             + Beloeb_EjerUdgift + Bynavn 
                             + Dist_raadhus + Dist_skole 
                             + Ejd_Altan 
                             + Ejd_AntalRum 
                             + Ejd_AntalToiletter + Ejd_Energimaerke
                             + I(Pris_EjdVurdering^(1/2))
                             + Sag_AnnonceretNettet + Salgstid
                             , data = home_flat)

summary_model <- summary(model_7)
variable_with_highest_p <- rownames(summary_model$coefficients)[which.max(summary_model$coefficients[, "Pr(>|t|)"])]
cat("Variable with highest p-value:", variable_with_highest_p, "\n")
cat("P-value for", variable_with_highest_p, ":", summary_model$coefficients[variable_with_highest_p, "Pr(>|t|)"], "\n")

model_8 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder 
                             + AntalStatusmoederSaelger + I(log(Areal_Bolig))  
                             + Areal_GarageCarport  
                             + Beloeb_EjerUdgift + Bynavn 
                             + Dist_raadhus + Dist_skole 
                             + Ejd_Altan 
                             + Ejd_AntalRum 
                             + Ejd_AntalToiletter + Ejd_Energimaerke
                             + I(Pris_EjdVurdering^(1/2))
                             + Sag_AnnonceretNettet + Salgstid
                             , data = home_flat)

summary_model <- summary(model_8)
variable_with_highest_p <- rownames(summary_model$coefficients)[which.max(summary_model$coefficients[, "Pr(>|t|)"])]
cat("Variable with highest p-value:", variable_with_highest_p, "\n")
cat("P-value for", variable_with_highest_p, ":", summary_model$coefficients[variable_with_highest_p, "Pr(>|t|)"], "\n")

levels(home_flat$Ejd_Energimaerke)[levels(home_flat$Ejd_Energimaerke)=="F"]<-"G" # This line removes a category from the categorical variable Energy label, in this case energy label C

model_9 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder 
                             + AntalStatusmoederSaelger + I(log(Areal_Bolig))  
                             + Areal_GarageCarport  
                             + Beloeb_EjerUdgift + Bynavn 
                             + Dist_raadhus + Dist_skole 
                             + Ejd_Altan 
                             + Ejd_AntalRum 
                             + Ejd_AntalToiletter + Ejd_Energimaerke
                             + I(Pris_EjdVurdering^(1/2))
                             + Sag_AnnonceretNettet + Salgstid
                             , data = home_flat)

summary_model <- summary(model_9)
variable_with_highest_p <- rownames(summary_model$coefficients)[which.max(summary_model$coefficients[, "Pr(>|t|)"])]
cat("Variable with highest p-value:", variable_with_highest_p, "\n")
cat("P-value for", variable_with_highest_p, ":", summary_model$coefficients[variable_with_highest_p, "Pr(>|t|)"], "\n")

levels(home_flat$Ejd_Energimaerke)[levels(home_flat$Ejd_Energimaerke)=="D"]<-"G" # This line removes a category from the categorical variable Energy label, in this case energy label C

model_10 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder 
                             + AntalStatusmoederSaelger + I(log(Areal_Bolig))  
                             + Areal_GarageCarport  
                             + Beloeb_EjerUdgift + Bynavn 
                             + Dist_raadhus + Dist_skole 
                             + Ejd_Altan 
                             + Ejd_AntalRum 
                             + Ejd_AntalToiletter + Ejd_Energimaerke
                             + I(Pris_EjdVurdering^(1/2))
                             + Sag_AnnonceretNettet + Salgstid
                             , data = home_flat)

summary_model <- summary(model_10)
variable_with_highest_p <- rownames(summary_model$coefficients)[which.max(summary_model$coefficients[, "Pr(>|t|)"])]
cat("Variable with highest p-value:", variable_with_highest_p, "\n")
cat("P-value for", variable_with_highest_p, ":", summary_model$coefficients[variable_with_highest_p, "Pr(>|t|)"], "\n")

model_11 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder 
                             + AntalStatusmoederSaelger + I(log(Areal_Bolig))  
                             + Areal_GarageCarport  
                             + Beloeb_EjerUdgift + Bynavn 
                             + Dist_raadhus + Dist_skole 
                             + Ejd_Altan 
                             + Ejd_AntalRum 
                             + Ejd_Energimaerke
                             + I(Pris_EjdVurdering^(1/2))
                             + Sag_AnnonceretNettet + Salgstid
                             , data = home_flat)

summary_model <- summary(model_11)
variable_with_highest_p <- rownames(summary_model$coefficients)[which.max(summary_model$coefficients[, "Pr(>|t|)"])]
cat("Variable with highest p-value:", variable_with_highest_p, "\n")
cat("P-value for", variable_with_highest_p, ":", summary_model$coefficients[variable_with_highest_p, "Pr(>|t|)"], "\n")

model_12 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder 
                             + AntalStatusmoederSaelger + I(log(Areal_Bolig))    
                             + Beloeb_EjerUdgift + Bynavn 
                             + Dist_raadhus + Dist_skole 
                             + Ejd_Altan 
                             + Ejd_AntalRum 
                             + Ejd_Energimaerke
                             + I(Pris_EjdVurdering^(1/2))
                             + Sag_AnnonceretNettet + Salgstid
                             , data = home_flat)

summary_model <- summary(model_12)
variable_with_highest_p <- rownames(summary_model$coefficients)[which.max(summary_model$coefficients[, "Pr(>|t|)"])]
cat("Variable with highest p-value:", variable_with_highest_p, "\n")
cat("P-value for", variable_with_highest_p, ":", summary_model$coefficients[variable_with_highest_p, "Pr(>|t|)"], "\n")

summary_model

anova(model_12 ,model)

#model 4

ggplot(model, aes(x=model$fitted.values, y=rstandard(model), col = factor(ifelse(abs(rstandard(model)) > 4, 2, 1))), fill=0) +
  geom_point(shape=1) +
  xlab("Fitted values") +
  ylab("Standardized residuals") +
  scale_color_manual(values = c("1" = "black", "2" = "#ff6347"), labels = c("Not Outliers", "Outliers")) +
  theme_minimal() +
  guides(col = guide_legend(title = NULL))

home_flat_80_model_reduction_AIC <- home_flat_80[home_flat_80$Ejd_Energimaerke != "Unknown", ] %>% 
pivot_wider(names_from = Ejd_Energimaerke, values_from = Ejd_Energimaerke, values_fn = length, values_fill = 0)

write.csv(home_flat_80_model_reduction_AIC, file = "home_flat_80_model_reduction_AIC.csv", row.names = FALSE)

# Model reduction AIC starting from model 3
model_AIC <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger 
                             + AntalStatusmoederSaelger + I(log(Areal_Bolig)) 
                             + Areal_Bolig_Commercial 
                             + Areal_GarageCarport + Areal_Kaelder 
                             + Beloeb_EjerUdgift + Bynavn 
                             + Dist_raadhus + Dist_skole 
                             + Ejd_Altan + Ejd_AntalPlan 
                             + Ejd_AntalRum + Ejd_AntalSoveVaerelser 
                             + Ejd_AntalToiletter + D	+ C +	G	+ F	+ E	
                             + A2020	+ A2015 + B	+ A2010
                             + I(Pris_EjdVurdering^(1/2))
                             + Sag_AnnonceretNettet + Salgstid
                             , data = home_flat_80_model_reduction_AIC)

step(model_AIC, direction = "backward")


model_AIC_done <- lm(formula = log(Pris_Salg) ~ Adresse_Etage + Alder + AntalStatusmoederSaelger +
    I(log(Areal_Bolig)) + Areal_GarageCarport + Beloeb_EjerUdgift + 
    Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalRum +
    Ejd_AntalToiletter + C + E + A2020 + A2015 + B + I(Pris_EjdVurdering^(1/2)) +
    Sag_AnnonceretNettet + Salgstid, data = home_flat_80_model_reduction_AIC)

summary(model_AIC_done)

#Model 3.3
model_3_3 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger 
                             + AntalStatusmoederSaelger + I(log(Areal_Bolig)) 
                             + Areal_Bolig_Commercial 
                             + Areal_GarageCarport + Areal_Kaelder 
                             + Beloeb_EjerUdgift + Bynavn 
                             + Dist_raadhus + Dist_skole 
                             + Ejd_Altan + Ejd_AntalPlan 
                             + Ejd_AntalRum + Ejd_AntalSoveVaerelser 
                             + Ejd_AntalToiletter + Ejd_Energimaerke
                             + I(Pris_EjdVurdering^(1/2))
                             + Sag_AnnonceretNettet + Salgstid
                             , data = model_3_data)


summary(model_12)

AIC(model_AIC_done)

summary(model_AIC_done, digits = 5)

AIC(model_12)

#############
#Model 3.3 Bacon test
#############

model_test <- lm(model_12$residuals^2 ~ Adresse_Etage + Alder 
                             + AntalStatusmoederSaelger + I(log(Areal_Bolig))    
                             + Beloeb_EjerUdgift + Bynavn 
                             + Dist_raadhus + Dist_skole 
                             + Ejd_Altan 
                             + Ejd_AntalRum 
                             + Ejd_Energimaerke
                             + I(Pris_EjdVurdering^(1/2))
                             + Sag_AnnonceretNettet + Salgstid
                             , data = home_flat)

summary(model_test)

nrow(home_flat)*0.05545 #Test statistic value
qchisq(.95, df=19)
bptest(model_12)

#############
#Model 3.3 Jarque-Bera test
#############

#Manual test
s_hat <- sum((model_12$residuals-mean(model_12$residuals))^3)/(length(model_12$residuals)*var(model_12$residuals)^(3/2))
k_hat <- sum((model_12$residuals-mean(model_12$residuals))^4)/(length(model_12$residuals)*var(model_12$residuals)^(2))
length(model_12$residuals)*(s_hat^2/6+(k_hat-3)^2/24)
qchisq(.95, df=2)

#R-test
JarqueBera.test(model_12$residuals)

# print(sort(rstandard(model))[1:3])

# outliers_model_1_knn <- c(285)

# Create a vector of colors for each observation
# colors <- rep("#3399cc", length(residuals(model)))
# colors[outliers_model_1_knn] <- "#ff6347"  # Set the color of outliers to red


# Plot the model residuals with outliers in red
#plot(model, which = 1, col = colors, pch = 20, id.n = 0)

#methods(plot)

levels(home_flat_20$Ejd_Energimaerke)[levels(home_flat_20$Ejd_Energimaerke)=="E"]<-"G" # This line removes a category from the categorical variable Energy label, in this case energy label C
levels(home_flat_20$Ejd_Energimaerke)[levels(home_flat_20$Ejd_Energimaerke)=="A2010"]<-"G" # This line removes a category from the categorical variable Energy label, in this case energy label C
levels(home_flat_20$Ejd_Energimaerke)[levels(home_flat_20$Ejd_Energimaerke)=="F"]<-"G" # This line removes a category from the categorical variable Energy label, in this case energy label C
levels(home_flat_20$Ejd_Energimaerke)[levels(home_flat_20$Ejd_Energimaerke)=="D"]<-"G" # This line removes a category from the categorical variable Energy label, in this case energy label C

pred_int <- predict(model_12, home_flat_20,
interval = "prediction")


# home_flat_20_model_reduction_AIC <- home_flat_20[home_flat_20$Ejd_Energimaerke != "Unknown", ] %>% 
# pivot_wider(names_from = Ejd_Energimaerke, values_from = Ejd_Energimaerke, values_fn = length, values_fill = 0)

# home_flat_20 <- home_flat_20_model_reduction_AIC
# pred_int <- predict(model_AIC_done, home_flat_20,
# interval = "prediction")


plot(pred_int[,1], log(home_flat_20$Pris_Salg),
xlab = "Fitted values",ylab = "Pris Salg")
abline(0, 1, lwd = 2, col = "blue")

in_pred_int <- sum(log(home_flat_20$Pris_Salg) >= pred_int[,2] &
log(home_flat_20$Pris_Salg) <= pred_int[,3])

in_pred_int / nrow(home_flat_20)

conf_int <- predict(model, home_flat_20,
interval = "confidence")

conf_int <- conf_int[order(conf_int[,1]),]
home_flat_20 <- home_flat_20[order(pred_int[,1]),]
pred_int <- pred_int[order(pred_int[,1]),]

plot(home_flat_20$Pris_Salg, ylab="Pris_Salg")
lines(1:nrow(home_flat_20), exp(pred_int[,1]),

col = "green", lwd = 2)

lines(1:nrow(home_flat_20), exp(pred_int[,2]),
col = "blue", lwd = 2)
lines(1:nrow(home_flat_20), exp(pred_int[,3]),
col = "blue", lwd = 2)
lines(1:nrow(home_flat_20), exp(conf_int[,2]),
col = "red", lwd = 2)
lines(1:nrow(home_flat_20), exp(conf_int[,3]),
col = "red", lwd = 2)

# Predict with the updated home_flat_20
pred_int <- predict(model_12, home_flat_20, interval = "prediction")

write.csv(pred_int, file = "pred_int.csv", row.names = FALSE)
write.csv(home_flat_20, file = "home_flat_20.csv", row.names = FALSE)

install.packages("Metrics")
library(Metrics)

# Specify the row IDs
row_ids_to_find <- c(151217)

# Find the row indices in the original dataset
row_indices <- match(row_ids_to_find, home_flat_20$row_id)

home_flat_20 <- home_flat_20[-row_indices, ]
# Access the $fit component

pred_int <- predict(model_12, home_flat_20,
interval = "prediction")

# Convert pred_int to a data frame
pred_int_df <- as.data.frame(pred_int)


rmse(log(home_flat_20$Pris_Salg), pred_int_df$fit)

rmse(home_flat_20$Pris_Salg, exp(pred_int_df$fit))

mae(log(home_flat_20$Pris_Salg), pred_int_df$fit)

mae(home_flat_20$Pris_Salg, exp(pred_int_df$fit))

mape(log(home_flat_20$Pris_Salg), pred_int_df$fit)

mape(home_flat_20$Pris_Salg, exp(pred_int_df$fit))