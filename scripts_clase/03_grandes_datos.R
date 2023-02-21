#Paquetes a utilizar en la lección 5

library(tidyverse)
library(lubridate)
library(rgdal)
library(leaflet)
library(readxl)
library(readr)
library(RCurl)
library(foreign)
library(geosphere)
library(sp)

#Cargamos los datos de las carpetas de investigacion
data_path <- paste0(getwd(), '/datos/da_carpetas-de-investigacion-pgj-cdmx.csv')
A <- read_csv(data_path)

#Solo nos quedamos con registros validos
B <- filter(A, latitud != 0 & longitud != 0)
rm(A)

# ---------------- Tiempo de procesamiento ------------------

# vector para guardar el tiempo de procesamiento auxiliar
tiempo <- c()

# 100 puntos
a <- now()

for (i in 1:100){
  distHaversine(
    c(B$longitud[i], B$latitud[i]),
    c(B$longitud[1],B$latitud[1]),
    r=6378137)
}

# Tiempo que el algoritmo tardo en correr
a <- now() - a
tiempo <- rbind(tiempo, a %>% as.numeric())

# 1,000 puntos
a <- now()
for (i in 1:1000){
  distHaversine(
    c(B$longitud[i], B$latitud[i]),
    c(B$longitud[1], B$latitud[1]),
    r=6378137)
}
# Tiempo que el algoritmo tardo en correr
a <- now() - a
tiempo <- rbind(tiempo, a %>% as.numeric())

# 10,000 puntos
a <- now()
for (i in 1:10000){
  distHaversine(
    c(B$longitud[i],B$latitud[i]),
    c(B$longitud[1],B$latitud[1]),
    r=6378137)
}

# Tiempo que el algoritmo tardo en correr
a <- now() - a
tiempo <- rbind(tiempo, a %>% as.numeric())

# 100,000 puntos
a <- now()
for (i in 1:100000){
  distHaversine(
    c(B$longitud[i],B$latitud[i]),
    c(B$longitud[1],B$latitud[1]),
    r=6378137)
}

# Tiempo que el algoritmo tardo en correr
a <- now() - a
tiempo <- rbind(tiempo,a %>% as.numeric())

# Graficando el tiempo de procesamiento
plot(tiempo)
lines(tiempo)

#¿Esta relacion se cumple también en el tiempo de procesamiento?
#¿El tiempo de procesamiento también aumenta 10 veces en cada ciclo?

#Proporcion entre ciclos contiguos
  #4.8 veces mas grande, 7.9 etc.
c(tiempo[2] / tiempo[1], tiempo[3] / tiempo[2], tiempo[4] / tiempo[3])


########################################################################
######## ¿Por qué crees que ocurre esto?

#Creciente de forma decreciente
plot(c(tiempo[2] / tiempo[1], tiempo[3] / tiempo[2], tiempo[4] / tiempo[3]))
lines(c(tiempo[2] / tiempo[1], tiempo[3] / tiempo[2], tiempo[4] / tiempo[3]))


















########################################################################
######## Optimización de cálculos

#Utilizando toda la base B, determinar cuantos delitos están a menos de 100 metros de los primeros 5 delitos

# Guardamos los datos en el vector total
total <- c()

a <- now()
for(i in 1:5){
  # Cuenta delitos totales
  vec <- 0
  
  for(j in 1:nrow(B)){
    if(
      (distHaversine(
        c(B$longitud[j], B$latitud[j]),
        c(B$longitud[i], B$latitud[i]),
        r=6378137) < 100)
      )
      # Encontramos un delito mas
      {vec <- vec + 1}
  }
  total <- rbind(total,vec) #Acumulamos los delitos cercanos por cada uno
  print(i)
}

#Medimos el tiempo de procesamiento
a_ciclo <- now() - a
a_ciclo





########################################################################
######## Método proyección UTM y valor absoluto
b <- now()

# Primero, convertimos todos los puntos a UTM Mercator
# Todos los puntos de la CDMX están en UTM zone 14
UTM <- '14'
d <- data.frame(lon=B$longitud, lat=B$latitud)
coordinates(d) <- c("lon", "lat")

# Estan en lat,lon entonces declaramos la proyeccion usual
sputm <- SpatialPoints(d, proj4string=CRS("+proj=longlat +datum=WGS84"))

# Todos los puntos viven en la CDMX
proyeccion <- CRS("+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs") 

# Transformamos los datos
spgeo <- spTransform(sputm, proyeccion)
spgeo <- as.data.frame(spgeo)

# Tenemos un equivalente como "lat,lon" en coordendas en el plano cartesiano
colnames(spgeo) <- c("lon_UTM","lat_UTM")

########################################################################
#Proceso valor absoluto

#Cargamos la distancia euclidiana
euclidean <- function(a, b) sqrt(sum((a - b)^2))

total2 <- c()#auxiliar para guardar

for(i in 1:5){
  aux <- 0#Contador de delitos dentro del buffer
  
  #Delitos dentro del cuadrado, con base en el valor absoluto
  vec1 <- which( abs(spgeo$lat_UTM[i] - spgeo$lat_UTM) < 100)
  vec2 <- which( abs(spgeo$lon_UTM[i] - spgeo$lon_UTM) < 100)
  
  #Si estan en ambos indices, implica que estan dentro del cuadrado de lado r y centro en c(spgeo$lat_UTM[i],spgeo$lon_UTM[i])
  vec <- intersect(vec1,vec2)
  
  #Ahora necesito descartar aquellos que están fuera del círculo de radio r y centro en c(spgeo$lat_UTM[i],spgeo$lon_UTM[i])
  if(length(vec) > 0){ #Solo verificamos que hay, al menos, un punto dentro del cuadrado
    for(j in 1:length(vec)){
      if( (euclidean(c(spgeo$lon_UTM[i],spgeo$lat_UTM[i]),c(spgeo$lon_UTM[vec[j]],spgeo$lat_UTM[vec[j]]))  < 100) ){
        aux <- aux + 1 #Confirmamos un delito dentro del circulo
      }
    }
  }
  #Acumulamos el numero de delitos en el buffer de cada uno
  total2 <- rbind(total2,aux)
  print(i)
}

#Medimos el tiempo de procesamiento
b_ciclo <- now() - b
b_ciclo


#####Notemos lo siguiente:
# ¿Por qué crees que ocurren estas diferencias?
cbind(total,total2) %>% View()


########################################################################
########################################################################
#¿Cómo podemos mejorar estos cálculos?

  #¿Qué podrías proponer como Director de Análisis? 










#Enfoque: cambiando la medida de distancia
total3 <- c()#auxiliar para guardar



for(i in 1:5){
  aux <- 0#Contador de delitos dentro del buffer
  
  #Delitos dentro del cuadrado, con base en el valor absoluto
  vec1 <- which(abs(spgeo$lat_UTM[i] - spgeo$lat_UTM) < 100)
  vec2 <- which(abs(spgeo$lon_UTM[i] - spgeo$lon_UTM) < 100)
  
  #Si estan en ambos indices, implica que estan dentro del cuadrado de lado r y centro en c(spgeo$lat_UTM[i],spgeo$lon_UTM[i])
  vec <- intersect(vec1,vec2)
  
  # Filtrar delitos en el cuadrado pero fuera del círculo
  if(length(vec) > 0){
    for(j in 1:length(vec)){
      if( (distHaversine(c(B$longitud[vec[j]],B$latitud[vec[j]]),c(B$longitud[i],B$latitud[i]), r=6378137) < 100) ){
        aux <- aux + 1 #Confirmamos un delito dentro del circulo
      }
    }
  }
  #Acumulamos el numero de delitos en el buffer de cada uno
  total3 <- rbind(total3,aux)
  print(i)
}


#Analicemos los resultados de forma completa
resultados <- data.frame(ciclo_hav = total, abs_utm = total2, abs_hav = total3)
#Por construcción (proyección), aún podría haber discrepancias, sin embargo en este caso coincide:
resultados %>% View()












########################################################################
########################################################################
# Shapefile to KML

#Leemos un shapefile de alcaldias
alcaldias <- paste0(getwd(), '/datos/Alcaldias/alcaldias.shp')
shape = readOGR(alcaldias, layer="alcaldias")

#Escribimos como KML
setwd("~/Downloads")
writeOGR(shape, dsn="alc.kml", layer="alc", driver="KML")


#Sube este archivo a:
  #MyMaps
  #Google Earth













#######################################################################################
#############          LEAFLET                          ###############################
#######################################################################################

#Leaflet es una librería de JavaScript pero adaptada (de gran forma para R y otros lenguajes)
#Leaflet fue creado en Ucrania
#https://leafletjs.com/

#Descarga el siguiente .csv 
#Enlace: https://www.dropbox.com/s/mokkc4lh19shjsc/restaurantes_cdmx.csv?dl=1

restaurantes_path <- paste0(getwd(), '/datos/restaurantes_cdmx.csv')
rest <- read_csv(restaurantes_path) %>% data.frame()
#rest <- read_csv("https://www.dropbox.com/s/mokkc4lh19shjsc/restaurantes_cdmx.csv?dl=1") %>% data.frame() #Alternativa con el URL

#Nos quedamos solo con algunos registros por fines practicos
rest <- rest[1:10,]

# Nuevas librerias
library(maps)
library(leaflet)


#######################################################################################
#############   AJUSTES DEL MAPA LEAFLET                       ########################


#############   ZOOM                                          ########################
#Creamos un objeto de leaflet y lo centramos en la Ciudad de México (o podemos agregar las coordenadas de otro lugar)

m <- leaflet() |>
  setView(lng = -99.21094624232052, lat = 19.390519038362434, zoom = 10)
m |> addTiles()

#Podemos cambiar el zoom inicial de nuestro mapa
#Muy cerca
m <- leaflet() %>% setView(lng = -99.21094624232052, lat = 19.390519038362434, zoom = 30)
m %>% addTiles()

#Muy lejos
m <- leaflet() %>% setView(lng = -99.21094624232052, lat = 19.390519038362434, zoom = 8)
m %>% addTiles()

#Regresamos al original
m <- leaflet() %>% setView(lng = -99.21094624232052, lat = 19.390519038362434, zoom = 10)
m  %>% addTiles()



#############   CAPA BASE DEL MAPA                       ########################

#Podemos utilizar libremente diseños de otros proveedores en leaflet
#Por default, Leaflet utiliza los de OpenStreetMap OSM (pero no son muy bonitos)

#En el siguiente enlace, podemos ver las distintas opciones de diseño y proveedores

#http://leaflet-extras.github.io/leaflet-providers/preview/index.html

#Utilizamos la capa Positron de CartoDB
m %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron)

#Capas de OSM
m %>%
  addTiles() %>%
  addProviderTiles(providers$OpenStreetMap.France)

#Color Negro
m %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.DarkMatter)

#Capa más estandar de Carto
m %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.VoyagerLabelsUnder)


#############   PUNTOS EN EL MAPA Y ETIQUETAS EN LEAFLET                 #######################

#Puntos en el mapa
#Creamos el objeto, declaramos los datos y el nombre de las columnas con las coordenadas
m <- leaflet(data = rest) %>% 
  addTiles() %>% 
  addMarkers(~longitud, ~latitud) %>% 
  addTiles() %>% 
  addProviderTiles(providers$CartoDB.VoyagerLabelsUnder)
m

#POP UP con el nombre del establecimiento
m <- leaflet(data = rest) %>% 
  addTiles() %>%
  addMarkers(~longitud, ~latitud, label = ~as.character(nom_estab)) %>% 
  addTiles() %>% addProviderTiles(providers$CartoDB.VoyagerLabelsUnder)
m

#CLICK con el nombre del establecimiento
m <- leaflet(data = rest) %>% addTiles() %>%
  addMarkers(~longitud, ~latitud, popup = ~as.character(nom_estab)) %>% 
  addTiles() %>% addProviderTiles(providers$CartoDB.VoyagerLabelsUnder)
m

#Podemos combinar ambas funcionalidades en el mismo, aunque no tiene mucho sentido
m <- leaflet(data = rest) %>% addTiles() %>%
  addMarkers(~longitud, ~latitud, popup = ~as.character(nom_estab),label = ~as.character(nom_estab)) %>% 
  addTiles() %>% addProviderTiles(providers$CartoDB.VoyagerLabelsUnder)
m


#############   PODEMOS PERSONALIZAR ICONOS CON ALGUNA TEMATICA                   #######################

#Icono de un tren en color verde
#Definimos el icono y sus caracteristicas
icon.glyphicon <- makeAwesomeIcon(
  icon= 'subway',
  markerColor = 'lightgreen',
  iconColor = 'black',
  library = 'fa')

#Lo aplicamos al mapa con la capa base de nuestra preferencia
m <- leaflet(data = rest) %>%
  addAwesomeMarkers(rest$longitud, lat = rest$latitud,
                    popup=rest$nom_estab,
                    icon = icon.glyphicon) %>% 
  addTiles() %>% addProviderTiles(providers$CartoDB.VoyagerLabelsUnder)
m

#Otra opción de icono...
#Icono de amazon en color rosa
#Definimos el icono y sus caracteristicas
icon.amazon <- makeAwesomeIcon(
  icon= 'amazon',
  markerColor = 'pink',
  iconColor = 'black',
  library = 'fa')

m <- leaflet(data = rest) %>%
  addAwesomeMarkers(rest$longitud, lat = rest$latitud,
                    popup=rest$nom_estab,
                    icon = icon.amazon) %>% 
  addTiles() %>% addProviderTiles(providers$CartoDB.VoyagerLabelsUnder)
m

#Podemos ver toda la lista de iconos disponibles para nuestro mapa y personalizar con preferencia
#https://fontawesome.com/v5.15/icons?d=gallery&p=2
#También es posible incorporar nuestro propios iconos.



#Ademas de los puntos, tambien podemos visualizar shapefiles

#############   SHAPES EN LEAFLET                   #######################
#En nuestro caso, utilizamos el shapefiles de las alcaldias
m <- leaflet(shape) %>%
  addPolygons() %>% 
  addTiles() %>% addProviderTiles(providers$CartoDB.VoyagerLabelsUnder)
m

#Vemos el nombre de los registros en la base de datos
View(shape@data)

#############   SUBCONJUNTO DEL SHAPE                 #######################

#Nos quedamos solo con algunos estados
shape_sub <- subset(
  shape,
  shape$nomgeo %in% c("Tlalpan","Iztapalapa")) 

#Pintamos el subconjunto
leaflet(shape_sub) %>%
  addPolygons() %>% 
  addTiles() %>%
  addProviderTiles(providers$CartoDB.VoyagerLabelsUnder)


# **La función subset será muy importante en tu vida como analista





#############   CLOROPHET POR NUMERO DE TELEVISIONES                 #######################
#Creamos una paleta de colores en color azul, en el dominion de una de las variables
pal <- colorNumeric(palette = "Blues", domain = shape$television)

#Muestra el numero de televisiones por estado, entre más azul, tenemos mas televisiones
m %>%
  addPolygons(stroke = FALSE,
              smoothFactor = 0.2,
              fillOpacity = 1,
              color = ~pal(television))


#Si queremos ver el nombre de la alcaldia
m %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
              color = ~pal(television),popup = ~as.character(nomgeo))







##############################################################################
#####  MOTION GIF      ########################################

#Nuevos paquetes
library(magick)
library(webshot)


#Movimiento en graficas

#Utiliza la siguiente matriz que representa el movimiento de personas en un mapa
master <- paste0(getwd(), '/datos/Master_20pasos_151.csv')
pasos <- read_csv(master)


#Revisa los siguientes videos:
#Video 1: https://www.youtube.com/watch?v=JEbSK51F6Ko&ab_channel=CarlosCastroCorrea
#Video 2: https://www.youtube.com/watch?v=WT7G6fQhHZQ&ab_channel=CarlosCastroCorrea

#Creamos un nuevo directorio para guardar las imagenes
dir_out <- "~/Downloads/ejemplo_clase"

#Analicemos la matriz:
View(head(pasos))

#Numero de personas

max(pasos$indice)

#Pasos de cada persona
max(pasos$iteracion)

####Analicemos el primer paso de todas las personas
p <- pasos %>%
  filter(iteracion == 1) %>%
  ggplot() +
  geom_point(aes(lat,lon)) 

p

####Analicemos el segundo paso de todas las personas
p <- pasos %>%
  filter(iteracion == 2) %>%
  ggplot() +
  geom_point(aes(lat,lon)) 

p

####Analicemos el tercer paso de todas las personas
p <- pasos %>%
  filter(iteracion == 3) %>%
  ggplot() +
  geom_point(aes(lat,lon)) 

p

####Analicemos el cuarto paso de todas las personas
p <- pasos %>%
  filter(iteracion == 4) %>%
  ggplot() +
  geom_point(aes(lat,lon)) 

p


#Podemos graficar los 151 pasos de cada persona y guardarlos en una imagen por paso:
for (i in 1:150){
  p <- pasos %>%
    filter(iteracion == i) %>%
    ggplot() +
    geom_point(aes(lat,lon)) 
  
  fp <- file.path(dir_out, paste0(i, ".png"))
  
  ggsave(plot = p, 
         filename = fp, 
         device = "png")
}

#Unimos las imagenes y hacemos una animación
imgs <- list.files(dir_out, full.names = TRUE)
img_list <- lapply(imgs, image_read)
img_joined <- image_join(img_list)

#Creamos la animacion con una funcion del paquete magick
img_animated <- image_animate(img_joined, fps = 2)

##Guardamos nuestra animacion
image_write(image = img_animated,
            path = "movimientos.gif")

#Buscamos el archivo
getwd()












