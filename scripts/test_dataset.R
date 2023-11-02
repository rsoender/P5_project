library(ggplot2)
load("/Users/rasmus/Library/CloudStorage/OneDrive-AalborgUniversitet/P5/P5 r/homedata.Rda")

sqm_price <- homedata$Pris_Salg / homedata$Areal_Bolig

boxplot_result <- boxplot.stats(homedata$Areal_Bolig)
boxplot_result
outliers <- boxplot_result$out
homedata_cleaned <- homedata[!homedata$Areal_Bolig %in% outliers, ]

boxplot_result_pris <- boxplot.stats(homedata_cleaned$Pris_Salg)
outliers_pris <- boxplot_result_pris$out
homedata_cleaned_pris <- homedata_cleaned[!homedata_cleaned$Pris_Salg %in% outliers_pris, ]

ggplot(homedata_cleaned_pris, aes(x = homedata_cleaned_pris$Areal_Bolig, y = homedata_cleaned_pris$Pris_Salg, stat_smooth(lm(homedata_cleaned_pris$Pris_Salg ~ homedata_cleaned_pris$Areal_Bolig, data = homedata_cleaned_pris)))) +
  geom_point() +
  labs(x = "Square Meters", y = "Price (in USD)", title = "Square Meters vs. Price") +
  theme_minimal()

model <- lm(homedata_cleaned_pris$Pris_Salg ~ homedata_cleaned_pris$Areal_Bolig, data = homedata_cleaned_pris)
model

write.csv(homedata, file = "/Users/rasmus/Library/CloudStorage/OneDrive-AalborgUniversitet/P5/P5 r/homedata.csv")
