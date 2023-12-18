#install.packages("skimr")
library(ggplot2)
library(mapview)
library(sf)
library(mapedit)
library(dplyr)
library(skimr)
library(data.table)
library(gridExtra)
library(patchwork)


load("C:/Users/signe/Documents/Uni/5. semester/Projekt/Kode/Data/Homedata.Rda")

home <- homedata

#Characteristics of the different years
table((subset(home, EjdType == "Ejerlejlighed" & Postnr %in% c(9000, 8000, 5000, c(1050:2450))))$Salgsaar)
home_f_2010 <- subset(home, (EjdType == "Ejerlejlighed" & Salgsaar == 2010 & home$Postnr %in% c(9000, 8000, 5000, c(1050:2450))))
home_f_2011 <- subset(home, (EjdType == "Ejerlejlighed" & Salgsaar == 2011 & home$Postnr %in% c(9000, 8000, 5000, c(1050:2450))))
home_f_2012 <- subset(home, (EjdType == "Ejerlejlighed" & Salgsaar == 2012 & home$Postnr %in% c(9000, 8000, 5000, c(1050:2450))))
home_f_2013 <- subset(home, (EjdType == "Ejerlejlighed" & Salgsaar == 2013 & home$Postnr %in% c(9000, 8000, 5000, c(1050:2450))))
home_f_2014 <- subset(home, (EjdType == "Ejerlejlighed" & Salgsaar == 2014 & home$Postnr %in% c(9000, 8000, 5000, c(1050:2450))))
home_f_2015 <- subset(home, (EjdType == "Ejerlejlighed" & Salgsaar == 2015 & home$Postnr %in% c(9000, 8000, 5000, c(1050:2450))))
home_f_2016 <- subset(home, (EjdType == "Ejerlejlighed" & Salgsaar == 2016 & home$Postnr %in% c(9000, 8000, 5000, c(1050:2450))))
home_f_2017 <- subset(home, (EjdType == "Ejerlejlighed" & Salgsaar == 2017 & home$Postnr %in% c(9000, 8000, 5000, c(1050:2450))))
home_f_2018 <- subset(home, (EjdType == "Ejerlejlighed" & Salgsaar == 2018 & home$Postnr %in% c(9000, 8000, 5000, c(1050:2450))))
home_f_2019 <- subset(home, (EjdType == "Ejerlejlighed" & Salgsaar == 2019 & home$Postnr %in% c(9000, 8000, 5000, c(1050:2450))))
home_f_2020 <- subset(home, (EjdType == "Ejerlejlighed" & Salgsaar == 2020 & home$Postnr %in% c(9000, 8000, 5000, c(1050:2450))))
home_f_2021 <- subset(home, (EjdType == "Ejerlejlighed" & Salgsaar == 2021 & home$Postnr %in% c(9000, 8000, 5000, c(1050:2450))))
home_f_2022 <- subset(home, (EjdType == "Ejerlejlighed" & Salgsaar == 2022 & home$Postnr %in% c(9000, 8000, 5000, c(1050:2450))))

#Counting the number of coloumns with NAs and so on
sum(colSums(is.na(home_f_2022)) > 0)
mean(colSums(is.na(home_f_2022)))


columns_with_nas <- colSums(is.na(home_f_2011)) > 0
average_nas_per_column_with_nas <- mean(colSums(is.na(home_f_2011[, ..columns_with_nas])))
print(paste("Average number of NAs per column for columns with NAs:", average_nas_per_column_with_nas))




#home_f is home with flats only in 2021
home_f <- subset(home, (EjdType == "Ejerlejlighed" & Salgsaar == 2021 & home$Postnr %in% c(9000, 8000, 5000, c(1050:2450))))

table(home_f$Adresse_Etage)
nrow(home_f)

#Number of rows in home_f where SagsStatus == "Endelig handel" or agsStatus == "Solgt"
nrow(home_f[home_f$SagsStatus == "Endelig handel" | home_f$SagsStatus == "Solgt",])

#Number of nas in ...
sum(is.na(home_f$Ejd_AntalToiletter))

#Number of ... in ...
sum(home_f$DWID_Projektsalg ==  "Ja" & !is.na(home_f$DWID_Projektsalg))

#home_f_uden_projekt is home_f without project sales
home_f_uden_projekt <- subset(home_f, (DWID_Projektsalg == "Nej"))

#Number of ... in ...
sum(home_f_uden_projekt$Ejd_Energimaerke ==  "Unknown" & !is.na(home_f_uden_projekt$Ejd_Energimaerke))


#Testing if the variable that represents the area of the lot makes sense
ggplot(data = home_f, aes(x = Areal_Grund)) +
  geom_histogram(bins = 100, color = "#3399cc", fill = "#3399cc") +
  theme_minimal() +
  labs(x = "Area of the lot", y = "Frequency") +
  xlim(0,10000)






# Cleaning
home_flat <- subset(home, (SagsStatus == "Solgt" | SagsStatus == "Endelig handel") & DWID_Projektsalg == "Nej" & EjdType == "Ejerlejlighed" & Salgsaar == 2021 & Postnr %in% c(9000, 8000, 5000, c(1050:2450)), select = c(Postnr, Bynavn, Beloeb_EjerUdgift, Pris_EjdVurdering, Pris_Salg, AntalFremvisninger, AntalStatusmoederSaelger, Sag_AnnonceretNettet, Areal_Bolig, Areal_Bolig_Commercial, Adresse_Etage, Areal_GarageCarport, Areal_Kaelder, Ejd_Altan, Ejd_AntalRum, Ejd_Energimaerke, Salgstid, Dist_skole, Alder, Hoejhus, Dist_raadhus, Ejd_AntalSoveVaerelser, Ejd_AntalToiletter, GisX_Wgs84, GisY_Wgs84, row_id))


#Looking at what NAs in number of bathrooms and number of toilets should be  

table(home_flat$Ejd_AntalToiletter)


# Changing NAs to 0 or 1
home_flat <- home_flat %>% mutate(across(c(Areal_Bolig_Commercial, Areal_Kaelder, Ejd_AntalSoveVaerelser, Ejd_AntalToiletter), ~ifelse(is.na(.), 0, .)))
home_flat <- home_flat %>% mutate(Ejd_AntalRum = ifelse(is.na(Ejd_AntalRum), 0, home_flat$Ejd_AntalRum))
  
# Changing Bynavn
home_flat <- home_flat %>% mutate(Bynavn = ifelse(Postnr %in% c(1050:2450), "København C", Bynavn))
home_flat <- home_flat %>% mutate(Bynavn = ifelse(Postnr == 9000, "Aalborg C", Bynavn))
  
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
  
  # Removing remaining NAs
  home_flat <- na.omit(home_flat)
  
nrow(home_flat)
  




#Decribtive statistics


#Table of Ejd_AntalPlan
table(home_flat$Ejd_AntalPlan)


# Create a ggplot scatter plot with Alder on the x-axis and Ejd_Opfoerelsesaar on the y-axis
ggplot(data = home_flat, aes(x = Alder, y = Ejd_Opfoerelsesaar)) +
  geom_point(color = "#3399cc") +
  theme_minimal()+
  labs(x = "Age", y =  "Construction year")

# Create a ggplot scatter plot with initial listing price on the x-axis and sales price on the y-axis
ggplot(data = home_flat, aes(x = Pris_FoersteUdbud, y = Pris_Salg)) +
  geom_point(color = "#3399cc") +
  theme_minimal()+
  labs(x = "Initial listing price", y =  "Sales price")

# Create a ggplot scatter plot with current listing price on the x-axis and sales price on the y-axis
ggplot(data = home_flat, aes(x = Pris_AktuelUdbud, y = Pris_Salg)) +
  geom_point(color = "#3399cc") +
  theme_minimal()+
  labs(x = "Current listing price", y =  "Sales price")

# Create a ggplot scatter plot with monthly gross payment on the x-axis and sales price on the y-axis
ggplot(data = home_flat, aes(x = Beloeb_MDBrutto, y = Pris_Salg)) +
  geom_point(color = "#3399cc") +
  theme_minimal()+
  labs(x = "Monthly gross payment", y =  "Sales price")

# Create a ggplot scatter plot with monthly net payment on the x-axis and sales price on the y-axis
ggplot(data = home_flat, aes(x = Beloeb_MDNetto, y = Pris_Salg)) +
  geom_point(color = "#3399cc") +
  theme_minimal()+
  labs(x = "Monthly net payment", y =  "Sales price")

# Create a ggplot scatter plot with down payment on the x-axis and sales price on the y-axis
ggplot(data = home_flat, aes(x = Beloeb_Udbetaling, y = Pris_Salg)) +
  geom_point(color = "#3399cc") +
  theme_minimal()+
  labs(x = "Down payment", y =  "Sales price")


ggplot(data = home_flat, aes(x = as.integer(Ejd_AntalPlan), y = Pris_Salg)) +
  geom_point(color = "#3399cc") +
  theme_minimal()+
  labs(x = "Floor number", y =  "Sales price")


