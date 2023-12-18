#########################
#Different help commands
#?plot.lm
#install.packages("")
#########################
library(class)
library(imputeTS)
library(ggplot2)
library(mapview)
library(sf)
library(mapedit)
library(dplyr)
library(skimr)
library(data.table)
library(gridExtra)
library(patchwork)
library(VIM)
library(xtable)
library(lmtest)
library(tsoutliers)
library(tidyr)



load("C:/Users/signe/Documents/Uni/5. semester/Projekt/Kode/Data/Homedata.Rda")

home <- homedata

home_f <-  subset(home, (SagsStatus == "Solgt" | SagsStatus == "Endelig handel") & DWID_Projektsalg == "Nej" & EjdType == "Ejerlejlighed" & Salgsaar == 2022 & Postnr %in% c(9000, 8000, 5000, c(1050:2450)), select = c(Pris_Salg, Postnr, Bynavn, Beloeb_EjerUdgift, Pris_EjdVurdering, AntalFremvisninger, AntalStatusmoederSaelger, Sag_AnnonceretNettet, Areal_Bolig, Areal_Bolig_Commercial, Adresse_Etage, Areal_GarageCarport, Areal_Kaelder, Ejd_Altan, Ejd_AntalRum, Ejd_Energimaerke, Salgstid, Dist_skole, Alder, Dist_raadhus, Ejd_AntalSoveVaerelser, Ejd_AntalToiletter, Ejd_AntalPlan, row_id, Adresse_Fuld))

table(home_f$Ejd_AntalSoveVaerelser)



#########################
# Cleaning
#########################
home_flat <- subset(home, (SagsStatus == "Solgt" | SagsStatus == "Endelig handel") & DWID_Projektsalg == "Nej" & EjdType == "Ejerlejlighed" & Salgsaar == 2021 & Postnr %in% c(9000, 8000, 5000, c(1050:2450)), select = c(Pris_Salg, Postnr, Bynavn, Beloeb_EjerUdgift, Pris_EjdVurdering, AntalFremvisninger, AntalStatusmoederSaelger, Sag_AnnonceretNettet, Areal_Bolig, Areal_Bolig_Commercial, Adresse_Etage, Areal_GarageCarport, Areal_Kaelder, Ejd_Altan, Ejd_AntalRum, Ejd_Energimaerke, Salgstid, Dist_skole, Alder, Dist_raadhus, Ejd_AntalSoveVaerelser, Ejd_AntalToiletter, Ejd_AntalPlan, row_id, Adresse_Fuld))

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
#Replace Unknown with NAs
home_flat$Ejd_Energimaerke <- as.factor(replace(home_flat$Ejd_Energimaerke, home_flat$Ejd_Energimaerke == "Unknown", NA))
#Replace A2 with NAs
home_flat$Ejd_Energimaerke <- as.factor(replace(home_flat$Ejd_Energimaerke, home_flat$Ejd_Energimaerke == "A2", NA))


# Changing Bynavn
home_flat <- home_flat %>%
  mutate(Bynavn = ifelse(Postnr %in% c(1050:2450), "København C", Bynavn))
home_flat <- home_flat %>%
  mutate(Bynavn = ifelse(Postnr == 9000, "Aalborg C", Bynavn))


table(home_flat$Ejd_Energimaerke)
sum(is.na(home_flat$Ejd_Energimaerke))


na_rows <- sum(apply(home_flat, 1, anyNA))
print(na_rows)

table(home_flat$Ejd_Energimaerke)


#########################
# Model 0
#########################

home_flat_model_0 <- na.omit(home_flat)


model_0 <- lm(log(Pris_Salg) ~ 1 , data = home_flat_model_0)
summary(model_0.1)

plot(model_0, which = 2, col = colors, pch = 20, id.n = 0, caption = NULL, cex.caption = 0.5)
par(col = "gray", lty = 1)
grid()




#########################
#KNN
#########################

n = nrow(home_flat)

k =  round(sqrt(n))

# Convert home_flat to data.frame
home_flat_df <- as.data.frame(home_flat)

#Find the variables with NAs
columns_with_NAs <- colnames(home_flat)[apply(home_flat, 2, anyNA)]

# Impute missing values using KNN and the Grower Metric (no weights depending on the distance) 
home_flat_imputed <- kNN(home_flat_df, k = k, weightDist = FALSE, metric = 'gower')

# Save home_flat as a csv file
#write.csv(home_flat, file = "home_flat.csv", row.names = FALSE)

# Save home_flat with imputed values as a csv file
#write.csv(home_flat_imputed, file = "home_flat_imputed.csv", row.names = FALSE)


# Put the imputed value in home_flat_imputed into home_flat
for (col in columns_with_NAs) {
  home_flat[[col]] <- home_flat_imputed[[col]]
}

# Convert home_flat back into a data.table
home_flat <- as.data.table(home_flat)

# Check if there are any NAs in home_flat
missing_values_after_imputation <- sum(is.na(home_flat))
print(paste("Number of missing values after imputation:", missing_values_after_imputation))

#Add column numbers to the observations in home_flat
home_flat_imputed$obs <- seq_len(nrow(home_flat)) 


#########################
# KNN plots
#########################

knn_Pris_EjdVurdering_plot <- ggplot(home_flat_imputed, aes(x = obs, y = Pris_EjdVurdering / 1000000, col = as.factor(Pris_EjdVurdering_imp), size = 1)) +
  geom_point(data = subset(home_flat_imputed, !Pris_EjdVurdering_imp), size = 1) +  # Size for blue points
  geom_point(data = subset(home_flat_imputed, Pris_EjdVurdering_imp), size = 2) +  # Size for orange points
  labs(x = "Observation index", y = "Property valuation (in millions DKK)") +
  scale_size(guide = "none") +  # Remove size legend
  scale_color_manual(name = "Property \nvaluation \nimputed", values = c("TRUE" = "#ff6347", "FALSE" = "#3399cc")) +  # Set colors and legend title
  theme_minimal()
#ggsave("knn_Pris_EjdVurdering_plot.pdf", plot = knn_Pris_EjdVurdering_plot, width = 8, height = 4.5)


knn_Alder_plot <- ggplot(home_flat_imputed, aes(x = obs, y = Alder, col = as.factor(Alder_imp), size = 1)) +
  geom_point(data = subset(home_flat_imputed, !Alder_imp), size = 1) +  # Size for blue points
  geom_point(data = subset(home_flat_imputed, Alder_imp), size = 2) +  # Size for orange points
  labs(x = "Observation index", y = "Age") +
  scale_size(guide = "none") +  # Remove size legend
  scale_color_manual(name = "Age \nimputed", values = c("TRUE" = "#ff6347", "FALSE" = "#3399cc")) +  # Set colors and legend title
  theme_minimal()
#ggsave("knn_Alder_plot.pdf", plot = knn_Alder_plot, width = 8, height = 4.5)

knn_Ejd_Energimaerke_plot <- ggplot(home_flat_imputed, aes(x = obs, y = Ejd_Energimaerke, col = as.factor(Ejd_Energimaerke_imp), size = 1)) +
  geom_point(data = subset(home_flat_imputed, !Ejd_Energimaerke_imp), size = 1) +  # Size for blue points
  geom_point(data = subset(home_flat_imputed, Ejd_Energimaerke_imp), size = 2) +  # Size for orange points
  labs(x = "Observation index", y = "Energy label") +
  scale_size(guide = "none") +  # Remove size legend
  scale_color_manual(name = "Energy \nlabel \nimputed", values = c("TRUE" = "#ff6347", "FALSE" = "#3399cc")) +  # Set colors and legend title
  theme_minimal()
#ggsave("knn_Ejd_Energimaerke_plot.pdf", plot = knn_Ejd_Energimaerke_plot, width = 8, height = 4.5)



#########################
# Making variables categorical and refactorisation
#########################

# Making Areal_Kaelder, Areal_GarageCarport, Areal_Bolig_Commercial, and Ejd_AntalPlan categorical, such that when the lm() function is used, dummy variables for each of the levels are created
home_flat$Areal_Kaelder <- as.factor(ifelse(home_flat$Areal_Kaelder > 0, 'Yes', 'No')) 

home_flat$Areal_GarageCarport <- as.factor(ifelse(home_flat$Areal_GarageCarport > 0, 'Yes', "No"))

home_flat$Areal_Bolig_Commercial <- as.factor(ifelse(home_flat$Areal_Bolig_Commercial > 0, 'Yes', "No"))

home_flat$Ejd_AntalPlan <- as.factor(ifelse(home_flat$Ejd_AntalPlan > 1, 'Multiple', "Single"))

# Refactor Energimaerke, AnnonceretNettet, Ejd_AltanEjd_AntalPlan, and Bynavn, such that the different variables have given reference groups when using lm()
home_flat$Ejd_Energimaerke <- factor(home_flat$Ejd_Energimaerke)
home_flat$Ejd_Energimaerke <- relevel(home_flat$Ejd_Energimaerke, ref = "G")
home_flat$Sag_AnnonceretNettet <- factor(home_flat$Sag_AnnonceretNettet)
home_flat$Sag_AnnonceretNettet <- relevel(home_flat$Sag_AnnonceretNettet, ref = "Nej")
home_flat$Ejd_Altan <- factor(home_flat$Ejd_Altan)
home_flat$Ejd_Altan <- relevel(home_flat$Ejd_Altan, ref = "Nej")
home_flat$Ejd_AntalPlan <- factor(home_flat$Ejd_AntalPlan)
home_flat$Ejd_AntalPlan <- relevel(home_flat$Ejd_AntalPlan, ref = "Single")
home_flat$Bynavn <- factor(home_flat$Bynavn)
home_flat$Bynavn <- relevel(home_flat$Bynavn, ref = "Aalborg C")


#########################
# Removing observations with typological errors found in Model 1
#########################
rows_to_remove <- c(144238, 142066, 139775, 139635, 145056)
home_flat_filtered <- subset(home_flat, !(row_id %in% rows_to_remove))

home_flat <- home_flat_filtered




#########################
# Removing 80% of the data to use the 20% for an out-of-sample test using KNN
#########################
set.seed(123)

rows_to_delete <- round(0.20 * nrow(home_flat))
indices_to_delete <- sample(seq_len(nrow(home_flat)), size = rows_to_delete, replace = FALSE)

home_flat_80 <- home_flat[-indices_to_delete, ]

home_flat_20 <- home_flat[indices_to_delete, ]


#making a home_flat_80 data set useble for AIC model specidication
home_flat_80_all_energy_labels <- home_flat_80[home_flat_80$Ejd_Energimaerke != "Unknown", ] %>% 
  pivot_wider(names_from = Ejd_Energimaerke, values_from = Ejd_Energimaerke, values_fn = length, values_fill = 0)

home_flat_20_all_energy_labels <- home_flat_20[home_flat_20$Ejd_Energimaerke != "Unknown", ] %>% 
pivot_wider(names_from = Ejd_Energimaerke, values_from = Ejd_Energimaerke, values_fn = length, values_fill = 0)






#########################
# Moddeling using KNN
#########################

#############
# Model 3.1
#############
model_3.1 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger + AntalStatusmoederSaelger + Areal_Bolig + Areal_Bolig_Commercial + Areal_GarageCarport + Areal_Kaelder + Beloeb_EjerUdgift + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalPlan + Ejd_AntalRum + Ejd_AntalSoveVaerelser + Ejd_AntalToiletter + Ejd_Energimaerke + Pris_EjdVurdering + Sag_AnnonceretNettet + Salgstid, data = home_flat_80)
summary(model_3.1)




#############
#Model 3.2
#############

#Inspecting observations/potential outliers of interest in Model 3.1
(sort(rstandard(model_3.1))[1:1])
outliers_model_3.1_knn <- c(285)  
home_flat_80_interest <- home_flat_80[c(285, 786, 840, 358, 1014, 161),]


#Plotting Model 3.1 with outliers in red and grey grid lines

# Create a vector of colors for each observation
colors <- rep("#3399cc", length(residuals(model_3.1)))
colors[outliers_model_3.1_knn] <- "#ff6347"  

plot(model_3.1, which = 2, col = colors, pch = 20, id.n = 0, caption = NULL, cex.caption = 0.5)
par(col = "gray", lty = 1)
grid()

#Removing the outlier
home_flat_80 <- home_flat_80[-c(285),]

model_3.2 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger + AntalStatusmoederSaelger + Areal_Bolig + Areal_Bolig_Commercial + Areal_GarageCarport + Areal_Kaelder + Beloeb_EjerUdgift + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalPlan + Ejd_AntalRum + Ejd_AntalSoveVaerelser + Ejd_AntalToiletter + Ejd_Energimaerke + Pris_EjdVurdering + Sag_AnnonceretNettet + Salgstid, data = home_flat_80)



#Plotting Model 3.2 without the outlier with grey grid lines

plot(model_3.2, which = 2, col = colors, pch = 20, id.n = 0, caption = NULL, cex.caption = 0.5)
par(col = "gray", lty = 1)
grid()




#############
#Model 3.3 (modification and no outiers) 
#############

model_3.3 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger + AntalStatusmoederSaelger + I(log(Areal_Bolig)) + Areal_Bolig_Commercial + Areal_GarageCarport + Areal_Kaelder + Beloeb_EjerUdgift + Bynavn  + Dist_raadhus + Dist_skole  + Ejd_Altan + Ejd_AntalPlan   + Ejd_AntalRum + Ejd_AntalSoveVaerelser + Ejd_AntalToiletter + Ejd_Energimaerke + I(Pris_EjdVurdering^(1/2)) + Sag_AnnonceretNettet + Salgstid , data = home_flat_80)

summary(model_3.3)


colors <- rep("#3399cc", length(residuals(model_1)))
colors[outliers_model_1_knn] <- "#ff6347"  

# Plot Model 3.3 
plot(model_3.3, which = 2, col = colors, pch = 20, id.n = 0, caption = NULL, cex.caption = 0.5)
# Add solid grid lines
par(col = "gray", lty = 1)
grid()


ggplot(model_3.3, aes(x=model_3.3$fitted.values, y=rstandard(model_3.3), col = factor(ifelse(abs(rstandard(model_3.3)) > 5, 2, 1)))) +
  geom_point(color="#3399cc") +
  xlab("Fitted values") +
  ylab("Standardised residuals") +
  theme_minimal() +
  guides(col = guide_legend(title = NULL)) + 
  geom_smooth(method = "loess", size = 0.5, se = FALSE, show.legend = FALSE)





#############
#Model 3.3 Bacon test
#############


#Manuel test
model_test_3.3 <- lm(model_3.3$residuals^2 ~ Adresse_Etage + Alder + AntalFremvisninger + AntalStatusmoederSaelger + I(log(Areal_Bolig)) + Areal_Bolig_Commercial + Areal_GarageCarport + Areal_Kaelder + Beloeb_EjerUdgift + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalPlan + Ejd_AntalRum + Ejd_AntalSoveVaerelser + Ejd_AntalToiletter + Ejd_Energimaerke + I(Pris_EjdVurdering^(1/2)) + Sag_AnnonceretNettet + Salgstid, data = home_flat_80)

summary(model_test_3.3)

nrow(home_flat_80)*0.08681 #Test statistic value
qchisq(.95, df=25)

#R-test
bptest(model_3.3)




#############
#Model 3.3 Jarque-Bera test
#############

#Manual test
s_hat <- sum((model_3.3$residuals-mean(model_3.3$residuals))^3)/(length(model_3.3$residuals)*var(model_3.3$residuals)^(3/2))
k_hat <- sum((model_3.3$residuals-mean(model_3.3$residuals))^4)/(length(model_3.3$residuals)*var(model_3.3$residuals)^(2))
length(model_3.3$residuals)*(s_hat^2/6+(k_hat-3)^2/24)
qchisq(.95, df=2)

#R-test
JarqueBera.test(model_4.1$residuals)



#############
#Model 4.1
#############

# Model reduction starting from model 3.3
model_3.3_1 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger 
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
                  , data = home_flat_80)
# Summary of the model_3.3
summary_model_3.3_1 <- summary(model_3.3_1)
# Extract the variable name with the highest p-value
variable_with_highest_p <- rownames(summary_model_3.3$coefficients)[which.max(summary_model_3.3$coefficients[, "Pr(>|t|)"])]
# Print the variable name with the highest p-value and its p-value
cat("Variable with highest p-value:", variable_with_highest_p, "\n")
cat("P-value for", variable_with_highest_p, ":", summary_model_3.3$coefficients[variable_with_highest_p, "Pr(>|t|)"], "\n")


#Removes a category from the categorical variable Energy label, in this case energy label E. Usen in the reference group.
levels(home_flat_80$Ejd_Energimaerke)[levels(home_flat_80$Ejd_Energimaerke)=="E"]<-"G" 

model_3.3_2 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger 
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
                  , data = home_flat_80)

summary_model_3.3_2 <- summary(model_3.3_2)

variable_with_highest_p <- rownames(summary_model_3.3_2$coefficients)[which.max(summary_model_3.3_2$coefficients[, "Pr(>|t|)"])]
cat("Variable with highest p-value:", variable_with_highest_p, "\n")
cat("P-value for", variable_with_highest_p, ":", summary_model_3.3_2$coefficients[variable_with_highest_p, "Pr(>|t|)"], "\n")



model_3.3_3 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger 
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
                  , data = home_flat_80)

summary_model_3.3_3 <- summary(model_3.3_3)

variable_with_highest_p <- rownames(summary_model_3.3_3$coefficients)[which.max(summary_model_3.3_3$coefficients[, "Pr(>|t|)"])]
cat("Variable with highest p-value:", variable_with_highest_p, "\n")
cat("P-value for", variable_with_highest_p, ":", summary_model_3.3_3$coefficients[variable_with_highest_p, "Pr(>|t|)"], "\n")

model_3.3_4 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger 
                  + AntalStatusmoederSaelger + I(log(Areal_Bolig))  
                  + Areal_GarageCarport  
                  + Beloeb_EjerUdgift + Bynavn 
                  + Dist_raadhus + Dist_skole 
                  + Ejd_Altan + Ejd_AntalPlan 
                  + Ejd_AntalRum + Ejd_AntalSoveVaerelser 
                  + Ejd_AntalToiletter + Ejd_Energimaerke
                  + I(Pris_EjdVurdering^(1/2))
                  + Sag_AnnonceretNettet + Salgstid
                  , data = home_flat_80)

summary_model_3.3_4 <- summary(model_3.3_4)
variable_with_highest_p <- rownames(summary_model_3.3_4$coefficients)[which.max(summary_model_3.3_4$coefficients[, "Pr(>|t|)"])]
cat("Variable with highest p-value:", variable_with_highest_p, "\n")
cat("P-value for", variable_with_highest_p, ":", summary_model_3.3_4$coefficients[variable_with_highest_p, "Pr(>|t|)"], "\n")

model_3.3_5 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger 
                  + AntalStatusmoederSaelger + I(log(Areal_Bolig))  
                  + Areal_GarageCarport  
                  + Beloeb_EjerUdgift + Bynavn 
                  + Dist_raadhus + Dist_skole 
                  + Ejd_Altan + Ejd_AntalPlan 
                  + Ejd_AntalRum  
                  + Ejd_AntalToiletter + Ejd_Energimaerke
                  + I(Pris_EjdVurdering^(1/2))
                  + Sag_AnnonceretNettet + Salgstid
                  , data = home_flat_80)

summary_model_3.3_5 <- summary(model_3.3_5)
variable_with_highest_p <- rownames(summary_model_3.3_5$coefficients)[which.max(summary_model_3.3_5$coefficients[, "Pr(>|t|)"])]
cat("Variable with highest p-value:", variable_with_highest_p, "\n")
cat("P-value for", variable_with_highest_p, ":", summary_model_3.3_5$coefficients[variable_with_highest_p, "Pr(>|t|)"], "\n")


model_3.3_6 <-lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger 
                 + AntalStatusmoederSaelger + I(log(Areal_Bolig))  
                 + Areal_GarageCarport  
                 + Beloeb_EjerUdgift + Bynavn 
                 + Dist_raadhus + Dist_skole 
                 + Ejd_Altan  
                 + Ejd_AntalRum  
                 + Ejd_AntalToiletter + Ejd_Energimaerke
                 + I(Pris_EjdVurdering^(1/2))
                 + Sag_AnnonceretNettet + Salgstid
                 , data = home_flat_80)

summary_model_3.3_6 <- summary(model_3.3_6)
variable_with_highest_p <- rownames(summary_model_3.3_6$coefficients)[which.max(summary_model_3.3_6$coefficients[, "Pr(>|t|)"])]
cat("Variable with highest p-value:", variable_with_highest_p, "\n")
cat("P-value for", variable_with_highest_p, ":", summary_model_3.3_6$coefficients[variable_with_highest_p, "Pr(>|t|)"], "\n")

# Remove a category from the categorical variable Energy label, in this case energy label A2010
levels(home_flat_80$Ejd_Energimaerke)[levels(home_flat_80$Ejd_Energimaerke)=="A2010"]<-"G" 

model_3.3_7 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder + AntalFremvisninger 
                  + AntalStatusmoederSaelger + I(log(Areal_Bolig))  
                  + Areal_GarageCarport  
                  + Beloeb_EjerUdgift + Bynavn 
                  + Dist_raadhus + Dist_skole 
                  + Ejd_Altan 
                  + Ejd_AntalRum 
                  + Ejd_AntalToiletter + Ejd_Energimaerke
                  + I(Pris_EjdVurdering^(1/2))
                  + Sag_AnnonceretNettet + Salgstid
                  , data = home_flat_80)

summary_model_3.3_7 <- summary(model_3.3_7)
variable_with_highest_p <- rownames(summary_model_3.3_7$coefficients)[which.max(summary_model_3.3_7$coefficients[, "Pr(>|t|)"])]
cat("Variable with highest p-value:", variable_with_highest_p, "\n")
cat("P-value for", variable_with_highest_p, ":", summary_model_3.3_7$coefficients[variable_with_highest_p, "Pr(>|t|)"], "\n")

model_3.3_8 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder 
                  + AntalStatusmoederSaelger + I(log(Areal_Bolig))  
                  + Areal_GarageCarport  
                  + Beloeb_EjerUdgift + Bynavn 
                  + Dist_raadhus + Dist_skole 
                  + Ejd_Altan 
                  + Ejd_AntalRum 
                  + Ejd_AntalToiletter + Ejd_Energimaerke
                  + I(Pris_EjdVurdering^(1/2))
                  + Sag_AnnonceretNettet + Salgstid
                  , data = home_flat_80)

summary_model_3.3_8 <- summary(model_3.3_8)
variable_with_highest_p <- rownames(summary_model_3.3_8$coefficients)[which.max(summary_model_3.3_8$coefficients[, "Pr(>|t|)"])]
cat("Variable with highest p-value:", variable_with_highest_p, "\n")
cat("P-value for", variable_with_highest_p, ":", summary_model_3.3_8$coefficients[variable_with_highest_p, "Pr(>|t|)"], "\n")

# Removes a category from the categorical variable Energy label, in this case energy label F
levels(home_flat_80$Ejd_Energimaerke)[levels(home_flat_80$Ejd_Energimaerke)=="F"]<-"G" 

model_3.3_9 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder 
                  + AntalStatusmoederSaelger + I(log(Areal_Bolig))  
                  + Areal_GarageCarport  
                  + Beloeb_EjerUdgift + Bynavn 
                  + Dist_raadhus + Dist_skole 
                  + Ejd_Altan 
                  + Ejd_AntalRum 
                  + Ejd_AntalToiletter + Ejd_Energimaerke
                  + I(Pris_EjdVurdering^(1/2))
                  + Sag_AnnonceretNettet + Salgstid
                  , data = home_flat_80)

summary_model_3.3_9 <- summary(model_3.3_9)
variable_with_highest_p <- rownames(summary_model_3.3_9$coefficients)[which.max(summary_model_3.3_9$coefficients[, "Pr(>|t|)"])]
cat("Variable with highest p-value:", variable_with_highest_p, "\n")
cat("P-value for", variable_with_highest_p, ":", summary_model_3.3_9$coefficients[variable_with_highest_p, "Pr(>|t|)"], "\n")

# Removes a category from the categorical variable Energy label, in this case energy label D
levels(home_flat_80$Ejd_Energimaerke)[levels(home_flat_80$Ejd_Energimaerke)=="D"]<-"G" #

model_3.3_10 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder 
                   + AntalStatusmoederSaelger + I(log(Areal_Bolig))  
                   + Areal_GarageCarport  
                   + Beloeb_EjerUdgift + Bynavn 
                   + Dist_raadhus + Dist_skole 
                   + Ejd_Altan 
                   + Ejd_AntalRum 
                   + Ejd_AntalToiletter + Ejd_Energimaerke
                   + I(Pris_EjdVurdering^(1/2))
                   + Sag_AnnonceretNettet + Salgstid
                   , data = home_flat_80)

summary_model_3.3_10 <- summary(model_3.3_10)
variable_with_highest_p <- rownames(summary_model_3.3_10$coefficients)[which.max(summary_model_3.3_10$coefficients[, "Pr(>|t|)"])]
cat("Variable with highest p-value:", variable_with_highest_p, "\n")
cat("P-value for", variable_with_highest_p, ":", summary_model_3.3_10$coefficients[variable_with_highest_p, "Pr(>|t|)"], "\n")

model_3.3_11 <-  lm(log(Pris_Salg) ~ Adresse_Etage + Alder 
                    + AntalStatusmoederSaelger + I(log(Areal_Bolig))  
                    + Areal_GarageCarport  
                    + Beloeb_EjerUdgift + Bynavn 
                    + Dist_raadhus + Dist_skole 
                    + Ejd_Altan 
                    + Ejd_AntalRum 
                    + Ejd_Energimaerke
                    + I(Pris_EjdVurdering^(1/2))
                    + Sag_AnnonceretNettet + Salgstid
                    , data = home_flat_80)

summary_model_3.3_11 <- summary(model_3.3_11)
variable_with_highest_p <- rownames(summary_model_3.3_11$coefficients)[which.max(summary_model_3.3_11$coefficients[, "Pr(>|t|)"])]
cat("Variable with highest p-value:", variable_with_highest_p, "\n")
cat("P-value for", variable_with_highest_p, ":", summary_model_3.3_11$coefficients[variable_with_highest_p, "Pr(>|t|)"], "\n")

model_3.3_12 <- lm(log(Pris_Salg) ~ Adresse_Etage + Alder 
                   + AntalStatusmoederSaelger + I(log(Areal_Bolig))
                   + Beloeb_EjerUdgift + Bynavn 
                   + Dist_raadhus + Dist_skole 
                   + Ejd_Altan 
                   + Ejd_AntalRum 
                   + Ejd_Energimaerke
                   + I(Pris_EjdVurdering^(1/2))
                   + Sag_AnnonceretNettet + Salgstid
                   , data = home_flat_80)

summary_model_3.3_12 <- summary(model_3.3_12)
variable_with_highest_p <- rownames(summary_model_3.3_12$coefficients)[which.max(summary_model_3.3_12$coefficients[, "Pr(>|t|)"])]
cat("Variable with highest p-value:", variable_with_highest_p, "\n")
cat("P-value for", variable_with_highest_p, ":", summary_model_3.3_12$coefficients[variable_with_highest_p, "Pr(>|t|)"], "\n")

summary_model_3.3_12


model_4.1 <- lm(formula = log(Pris_Salg) ~ Adresse_Etage + Alder + AntalStatusmoederSaelger + I(log(Areal_Bolig)) + Beloeb_EjerUdgift + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalRum + Ejd_Energimaerke + I(Pris_EjdVurdering^(1/2)) + Sag_AnnonceretNettet + Salgstid, data = home_flat_80)


#F-test to determine whether there is any significace between Model 4.1 and Model 3.3. 
anova (model_3.3, model_4.1)



#############
#Plotting of Model 4.1
#############

#Potential outliers
(sort(rstandard(model_4.1))[1:3])

# Plot model_1s residuals with outliers in red and add solid grid lines
colors <- rep("#3399cc", length(residuals(model_4.1)))
plot(model_4.1, which = 3, col = colors, pch = 20, id.n = 0, caption = NULL, cex.caption = 0.5)
par(col = "gray", lty = 1)
grid()



#Standadaized residuals vs fitted
ggplot(model_4.1, aes(x=model_4.1$fitted.values, y=rstandard(model_4.1), col = factor(ifelse(abs(rstandard(model_4.1)) > 5, 2, 1)))) +
  geom_point(color="#3399cc") +
  xlab("Fitted values") +
  ylab("Standardised residuals") +
  theme_minimal() +
  guides(col = guide_legend(title = NULL)) + 
  geom_smooth(method = "loess", size = 0.5, se = FALSE, show.legend = FALSE)


#############
#Model 4.1 Bacon test
#############

model_test_4.1 <- lm(model_4.1$residuals^2 ~ Adresse_Etage + Alder + AntalStatusmoederSaelger + I(log(Areal_Bolig)) + Beloeb_EjerUdgift + Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalRum + Ejd_Energimaerke + I(Pris_EjdVurdering^(1/2)) + Sag_AnnonceretNettet + Salgstid, data = home_flat_80)

summary(model_test_4.1)

nrow(home_flat_80)*0.05607 #Test statistic value
qchisq(.95, df=19) #Df = number of parameters minus the intercept
bptest(model_4.1)




#############
#Model 4.1 Jarque-Bera test
#############

#Manual test
s_hat <- sum((model_3.3$residuals-mean(model_3.3$residuals))^3)/(length(model_3.3$residuals)*var(model_3.3$residuals)^(3/2))
k_hat <- sum((model_3.3$residuals-mean(model_3.3$residuals))^4)/(length(model_3.3$residuals)*var(model_3.3$residuals)^(2))
length(model_3.3$residuals)*(s_hat^2/6+(k_hat-3)^2/24)
qchisq(.95, df=2)





#############
#Model 4.2
#############

# Model specification using AIC starting from model 3
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
                , data = home_flat_80_all_energy_labels)

step(model_AIC, direction = "backward")

model_4.2 <- lm(formula = log(Pris_Salg) ~ Adresse_Etage + Alder + AntalStatusmoederSaelger +
                       I(log(Areal_Bolig)) + Areal_GarageCarport + Beloeb_EjerUdgift + 
                       Bynavn + Dist_raadhus + Dist_skole + Ejd_Altan + Ejd_AntalRum +
                       Ejd_AntalToiletter + C + E + A2020 + A2015 + B + I(Pris_EjdVurdering^(1/2)) +
                       Sag_AnnonceretNettet + Salgstid, data = home_flat_80_all_energy_labels)

AIC(model_3.3)

AIC(model_4.2)

AIC(model_4.1)




#########################
#Prediction intervals 
#########################


#############
#Model 4.1
#############

#Putting the energy labels A2010, D, E, F energyin the reference group for energy labels in home_flat_20
levels(home_flat_20$Ejd_Energimaerke)[levels(home_flat_20$Ejd_Energimaerke)=="A2010"]<-"G" 
levels(home_flat_20$Ejd_Energimaerke)[levels(home_flat_20$Ejd_Energimaerke)=="D"]<-"G" 
levels(home_flat_20$Ejd_Energimaerke)[levels(home_flat_20$Ejd_Energimaerke)=="E"]<-"G" 
levels(home_flat_20$Ejd_Energimaerke)[levels(home_flat_20$Ejd_Energimaerke)=="F"]<-"G" 


#Making the prediction interval for model 4.1 on the 20%
pred_int_4.1 <- predict(model_4.1, home_flat_20,
                    interval = "prediction")


#Plot the predictions against the real sales prices against a straight line
plot(pred_int_4.1[,1], log(home_flat_20$Pris_Salg), xlab = "Fitted values", ylab = "Sales price", col = "#3399cc", pch = 20,)
par(col = "gray", lty = 1)
grid()
abline(0, 1, lwd = 2, col = "#ff6347")


#The sum and percentage of all the observations in the prediction intervals
in_pred_int_4.1 <- sum(log(home_flat_20$Pris_Salg) >= pred_int_4.1[,2] &
                     log(home_flat_20$Pris_Salg) <= pred_int_4.1[,3])

in_pred_int_4.1 / nrow(home_flat_20) 


#Making the confidence intervals for model 1 no outlers on the 20%
conf_int_4.1 <- predict(model_1_no_outliers, home_flat_20,
                    interval = "confidence")


#Order the confidence interval
conf_int_4.1 <- conf_int_4.1[order(conf_int_4.1[,1]),]

#Order home_flat_20 wrt the pred_int_4.1
home_flat_20 <- home_flat_20[order(pred_int_4.1[,1]),]

#Order pred_int wrt the pred_int
pred_int <- pred_int[order(pred_int_4.1[,1]),]

#Plot the prediction and confidence intervals against the sales prices 
plot(home_flat_20$Pris_Salg, ylab="Sales price", col = "#3399cc", pch = 20) #plots the dots
lines(1:nrow(home_flat_20), exp(pred_int_4.1[,1]), col = "#ff6347", lwd = 2) #what the model predicts
par(col = "gray", lty = 1)
grid()

lines(1:nrow(home_flat_20), exp(pred_int_4.1[,2]),
      col = "chocolate1", lwd = 2)
lines(1:nrow(home_flat_20), exp(pred_int_4.1[,3]),
      col = "chocolate1", lwd = 2)
lines(1:nrow(home_flat_20), exp(conf_int_4.1[,2]),
      col = "burlywood4", lwd = 2)
lines(1:nrow(home_flat_20), exp(conf_int_4.1[,3]),
      col = "burlywood4", lwd = 2)






#############
#Model 4.2
#############

#Making the prediction interval for model 4.2 on the 20%
pred_int_4.2 <- predict(model_4.2, home_flat_20_all_energy_labels,
                    interval = "prediction")

pred_int_4.2
#Plot the predictions against the real sales prices against a straight line
plot(pred_int_4.2[,1], log(home_flat_20_all_energy_labels$Pris_Salg), xlab = "Fitted values", ylab = "Sales price", col = "#3399cc", pch = 20,)
par(col = "gray", lty = 1)
grid()
abline(0, 1, lwd = 2, col = "#ff6347")


#The sum and percentage of all the observations in the prediction intervals
in_pred_int_4.2 <- sum(log(home_flat_20_all_energy_labels$Pris_Salg) >= pred_int_4.2[,2] &
                     log(home_flat_20_all_energy_labels$Pris_Salg) <= pred_int_4.2[,3])

in_pred_int_4.2 / nrow(home_flat_20_all_energy_labels) 


#Making the confidence intervals for model 1 no outlers on the 20%
conf_int <- predict(model_4.2, home_flat_20_all_energy_labels,
                    interval = "confidence")


#Order the confidence interval
conf_int <- conf_int[order(conf_int[,1]),]

#Order home_flat_20 wrt the pred_int_4.2
home_flat_20_all_energy_labels <- home_flat_20_all_energy_labels[order(pred_int_4.2[,1]),]

#Order pred_int_4.2 wrt the pred_int_4.2
pred_int_4.2 <- pred_int_4.2[order(pred_int_4.2[,1]),]

#Plot the prediction and confidence intervals against the sales prices 
plot(home_flat_20_all_energy_labels$Pris_Salg, ylab="Sales price", col = "#3399cc", pch = 20) #plots the dots
lines(1:nrow(home_flat_20_all_energy_labels), exp(pred_int_4.2[,1]), col = "#ff6347", lwd = 2) #what the model predicts
par(col = "gray", lty = 1)
grid()

lines(1:nrow(home_flat_20_all_energy_labels), exp(pred_int_4.2[,2]),
      col = "chocolate1", lwd = 2)
lines(1:nrow(home_flat_20_all_energy_labels), exp(pred_int_4.2[,3]),
      col = "chocolate1", lwd = 2)
lines(1:nrow(home_flat_20_all_energy_labels), exp(conf_int[,2]),
      col = "burlywood4", lwd = 2)
lines(1:nrow(home_flat_20), exp(conf_int[,3]),
      col = "burlywood4", lwd = 2)







