library(ggplot2)

load(
  "/Users/rasmus/Library/CloudStorage/OneDrive-AalborgUniversitet/P5/P5 r/home.Rda"
)

home <- homedata

home_9000 <- home[home$Postnr == 9000, ]
home_aarhus <- home[home$Postnr == 8000,]
home_odense <- home[home$Postnr == 5000,]
home_copenhagen <- home[home$Postnr %in% c(1051:2450),]


boxplot(home_9000$Pris_Salg, home_aarhus$Pris_Salg, home_odense$Pris_Salg, home_copenhagen$Pris_Salg)
