library(ggplot2)

load(
  "/Users/rasmus/Library/CloudStorage/OneDrive-AalborgUniversitet/P5/P5 r/home.Rda"
)

home <- homedata

# Area definitions (not cleaned)
home_aalborg <- home[home$Postnr == 9000,]
home_aarhus <- home[home$Postnr == 8000,]
home_odense <- home[home$Postnr == 5000,]
home_copenhagen <- home[home$Postnr %in% c(1051:2450),] # 1473 or 2450

# Scatter plot
ggplot(home_9000, aes(x = home_9000$Areal_Bolig, y = home_9000$Pris_Salg)) + geom_point(size =2,shape = ".",alpha = 0.9) + xlim(25, 200) + ylim(0, 6 * 10 ^ 6) + theme_minimal() + xlab("Area") + ylab("Price") + stat_smooth(method = "lm", formula = y ~ x, geom = "smooth")

