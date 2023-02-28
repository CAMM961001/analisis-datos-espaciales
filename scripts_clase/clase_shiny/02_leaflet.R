## Script de leaflet

library(leaflet)
library(leaflet.extras)
library(rgdal)
library(readr)
library(tidyverse)
# Ejemplo marcador


m <- leaflet() %>%
  addTiles() %>%  # Añade por defecto los Tiles de  OpenStreetMap
  addMarkers(lat=19.432386674080124, lng=-99.13170582825103, 
             popup="Palacio Nacional")
m  # Imprime el mapa

## Ejemplo base de datos

denue <- read.csv("denue_inegi_61_.csv", fileEncoding = "latin1") 
denue <- denue %>% filter(nombre_act=="Escuelas de deporte del sector privado") %>% 
  filter(entidad=="Aguascalientes") %>% 
  select(nom_estab, latitud,longitud) 

m <- leaflet() %>%
  addTiles() %>%  # Añade por defecto los Tiles de  OpenStreetMap
  addMarkers(lat=denue$latitud, lng=denue$longitud, 
             popup=denue$nom_estab)
m  # Imprime el mapa

# Ejemplo Shapefile

mapa_estados <- readOGR( 
  dsn= "estados" , 
  verbose=FALSE
)

leaflet() %>% 
  addProviderTiles(providers$CartoDB.Voyager) %>% 
  addPolygons(
    data=mapa_estados,
    stroke = TRUE, fillOpacity = 10,
    color ='white', 
    fillColor="blue",
    popup=mapa_estados@data$ESTADO
    
  ) 

# Ejemplo Shapefile Colorear

mapa_estados@data$poblacion <- sample(300:1000, nrow(mapa_estados@data), replace=TRUE)
mapa_estados@data

colores <- colorBin("Greens",
                    mapa_estados@data$poblacion,
                    bins = 3,
                    na.color = "#ffffff1C")


leaflet() %>% 
  addProviderTiles(providers$CartoDB.Voyager) %>% 
  addPolygons(
    data=mapa_estados,
    stroke = TRUE, fillOpacity = 10,
    color ='white', 
    fillColor=~colores(poblacion),
    popup=paste0("Estado: ",mapa_estados@data$ESTADO ,"<br> Población: ", mapa_estados@data$poblacion)
    
  ) 


##Varias capas

leaflet() %>% 
  addProviderTiles(providers$CartoDB.Voyager) %>% 
  addPolygons(
    data=mapa_estados,
    stroke = TRUE, fillOpacity = 10,
    color ='white', 
    fillColor=~colores(poblacion),
    popup=paste0("Estado: ",mapa_estados@data$ESTADO ,"<br> Población: ", mapa_estados@data$poblacion)
    
  )  %>%  # Añade por defecto los Tiles de  OpenStreetMap
  addMarkers(lat=denue$latitud, lng=denue$longitud, 
             popup=denue$nom_estab)


