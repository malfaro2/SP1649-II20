library(knitr)
library(sp)
library(raster)
library(rgdal)
library(dismo)
library(XML)
library(dplyr)
library(sf)
library(spData)
library(here)
library(ggplot2)
library(ggspatial)
library(tmap)
library(RcmdrMisc)

Base <- read.csv("DATA_NEW.csv", header= T, sep = ";", dec = ".")
View(Base)
names(Base) <- c("MINAE",names(Base)[2:14],
                 "estMINAE",names(Base)[16:17])

library(leafletR)
toGeoJSON(data=Base, dest=here(), lat.lon=c(16,17))
Base<- st_read("Base.geojson")
View(Base)
CostaRica <- st_read("Distritos_de_Costa_Rica.geojson")

CostaRica <-CostaRica[-c(1:29),] # Para eliminar la isla del coco

#View(CostaRica)
plot(CostaRica$geometry)
plot(Base$geometry, col="red", cex=0.5,add=TRUE) 


#Plotear las estaciones de servicio en el mapa... Algunas quedan fuera del mapa (problemas de captura de los datos).
proj <- CRS("+proj=utm +zone=17 +datum=WGS84")
plot(CostaRica$geometry) 
plot(Base$geometry, 
     col = "red",  # color
     cex = 0.5,    # size of symbol
     add = TRUE)

#Otro gr?fico
ggplot() +  geom_sf(data = CostaRica$geometry) +
  ggtitle("Estaciones de Servicio", subtitle = "Distribuci?n en Costa Rica ") + # a ggplot function 
  geom_sf(data = Base,# precise that it will be a spatial geometry
          aes(size = Base$Total_2017/10000,
              color = 'red', alpha = 1/5,# provide some aesthetics
              geometry = geometry,    # the geometry column (usually auto detected)
              fill = CostaRica$nom_prov)       # we want the polygon color to change following the count
  ) -> g # store it in g

g

Base$Total_2017 <- as.numeric(as.character(Base$Total_2017))
Base$Islas <- as.numeric(as.character(Base$Islas))
Base$Mangueras <- as.numeric(as.character(Base$Mangueras))

Base <- Base %>% st_join(CostaRica %>% select(NOM_PROV))

Base_prov <- Base %>%
  filter(!is.na(NOM_PROV)) %>% # remove NAs
  st_drop_geometry() %>% # let's put geometry aside
  group_by(NOM_PROV) %>%  # group data by GSS_CODE
  tally(name = "count", sort= TRUE) # Aggregate
Base_prov

Base_prov_sum <- Base %>%
  filter(!is.na(NOM_PROV)) %>% # remove NAs
  st_drop_geometry() %>% # let's put geometry aside
  group_by(NOM_PROV) %>%  # group data by GSS_CODE
  summarise(sum = sum(Total_2017,na.rm=TRUE), count = n()) # Aggregate
Base_prov_sum


boroughs_centroids <- CostaRica %>%
  select(NOM_PROV, NOM_CANT) %>% # only keep useful columns
  st_centroid()

CostaRica %>% left_join(Base_canton_sum) %>%
  st_write(
    dsn = here("foss4g_R_workshop.gpkg"),
    layer = "CR_27700", 
    layer_options = "OVERWRITE=true")


boroughs_centroids %>%
  left_join(Base_canton_sum) %>%
  st_write(
    dsn = here("foss4g_R_workshop.gpkg"),
    layer = "boroughs_centroids_27700", 
    layer_options = "OVERWRITE=true")


Base %>%
  left_join(Base_canton_sum) %>%
  st_write(
    dsn = here("foss4g_R_workshop.gpkg"),
    layer = "cycle_hire_27700",
    layer_options = "OVERWRITE=true")


boroughs_centroids %>%
  left_join(Base_canton_sum ) %>%
  st_write(
    dsn = here("foss4g_R_workshop.gpkg"),
    layer = "boroughs_centroids_27700", 
    layer_options = "OVERWRITE=true")


Base %>%
  left_join(Base_canton_sum ) %>%
  st_write(
    dsn = here("foss4g_R_workshop.gpkg"),
    layer = "london_boroughs_27700 ",
    layer_options = "OVERWRITE=true")
#ggs_code=canton

london_27700 %>% left_join(cycle_hire_by_area_sum) %>%
  st_write(
    dsn = here("foss4g_R_workshop.gpkg"),
    layer = "london_boroughs_27700", 
    layer_options = "OVERWRITE=true")

st_layers(here("foss4g_R_workshop.gpkg"))



CostaRica %>% # pipe data to
  ggplot() +                # a ggplot function
  geom_sf(                # precise that it will be a spatial geometry
    aes(                  # provide some aesthetics
      geometry = geom,    # the geometry column (usually auto detected)
      fill = count)       # we want the polygon color to change following the count
  ) -> g # store it in g

g # display g

