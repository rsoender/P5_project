library(ggplot2)

load("/Users/rasmus/Library/CloudStorage/OneDrive-AalborgUniversitet/P5/P5 r/home.Rda")

home <- homedata

home_aalborg <- home[home$Postnr == 9000,]
home_copenhagen <- home[home$Postnr %in% c(1051:2450),]

ggplot(
  home_aalborg, aes(x=home_aalborg$Pris_Salg)) + geom_histogram(bins=100) + xlim(0,15*10^6)

ggplot(
  home_copenhagen, aes(x=home_copenhagen$Pris_Salg)) + geom_histogram(bins=100) + xlim(0,15*10^6)

