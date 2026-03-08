if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  terra,
  giscoR,
  sf,
  tidyverse,
  ggtern,
  elevatr,
  png,
  rayshader,
  magick,
  furrr,
  here,
  future,
  rvest,
  RSelenium,
  netstat,
  janitor,
  extrafont,
  rnaturalearth,
  glue,
  exactextractr
)

ukraine_oblasts <- read_sf("data/geoBoundaries-UKR-ADM1.shp") |> 
  mutate(shapeName = case_when(
    shapeName == "Chernihiv" ~ "Chernihivska",
    shapeName == "Volyn" ~ "Volynska",
    shapeName == "Rivne" ~ "Rivnenska",
    shapeName == "Zhytomyr" ~ "Zhytomyrska",
    shapeName == "Kiev" ~ "Kyivska",
    shapeName == "Transcarpathia" ~ "Zakarpatska",
    shapeName == "Chernivtsi" ~ "Chernivetska",
    shapeName == "Ivano-Frankivs'k" ~ "Ivano-Frankivska",
    shapeName == "Odessa" ~ "Odeska",
    shapeName == "Vinnytsya" ~ "Vinnytska",
    shapeName == "L'viv" ~ "Lvivska",
    shapeName == "Sumy" ~ "Sumska",
    shapeName == "Kharkiv" ~ "Kharkivska",
    shapeName == "Luhans'k" ~ "Luhanska",
    shapeName == "Donets'k" ~ "Donetska",
    shapeName == "Kherson" ~ "Khersonska",
    shapeName == "Zaporizhzhya" ~ "Zaporizka",
    shapeName == "Mykolayiv" ~ "Mykolaivska",
    shapeName == "Poltava" ~ "Poltavska",
    shapeName == "Khmel'nyts'kyy" ~ "Khmelnytska",
    shapeName == "Ternopil'" ~ "Ternopilska",
    shapeName == "Dnipropetrovs'k" ~ "Dnipropetrovska",
    shapeName == "Cherkasy" ~ "Cherkaska",
    shapeName == "Kirovohrad" ~ "Kirovohradska",
    shapeName == "Kiev City" ~ "Kyiv",
    shapeName == "Kiev City" ~ "Kyiv",
    TRUE ~ shapeName  # Keep all other values as they are
  ))

world_shapes <- read_sf("data/geoBoundaries-UKR-ADM1.shp")
head(world_shapes)
world_shapes |> 
  select(shapeID, shapeName, shapeISO, geometry) |> 
  head(7)
unique(world_shapes$shapeName)

ggplot() +
  geom_sf(data = world_shapes) +
  coord_sf(crs = "+proj=ccon +lat_1=52 +lon_0=19")


# country_sf <- read_sf("data/geoBoundaries-UKR-ADM1.shp") |> 
#   filter(shapeISO %in% c("UA-65", "UA-12", "UA-23","UA-8000"))
# 
# ggplot(country_sf) +
#   geom_sf() +
#   theme_minimal()

data<- read_csv("D:/RProjects/r4ds-2025/Homeworks/data/Work_conditions.csv")
head(data)
# glimpse(data)
# summary(data)
