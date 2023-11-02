library(mapview)
library(sf)
library(mapedit)
# remotes::install_github("r-spatial/mapview")

load("/Users/rasmus/Library/CloudStorage/OneDrive-AalborgUniversitet/P5/P5 r/home.Rda")

home <- homedata

# home_2 <- subset(home, home$SagsStatus != "Under salg")

# write.csv(home_2, file = "/Users/rasmus/Library/CloudStorage/OneDrive-AalborgUniversitet/P5/P5 r/home_2.csv")

# home_aalborg_coords <- home[home$Postnr == 9000,  c("GisX_Wgs84", "GisY_Wgs84")]

home_aalborg <- st_as_sf(home[home$Postnr == 9000, ], coords = c("GisX_Wgs84", "GisY_Wgs84"), crs = 4269)
aalborg <- mapview(home_aalborg, map.types = "OpenStreetMap")

home_aalborg_pol <- drawFeatures(aalborg)
home_aalborg_pol2 <- st_transform(home_aalborg_pol, crs = 4269)

aalborg_c <- st_intersects(home_aalborg_pol2, home_aalborg)[[1]]
home_aalborg_c <- home_aalborg[aalborg_c,]

mapview(home_aalborg[aalborg_c,], map.types = "OpenStreetMap")
