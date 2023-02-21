#Aclaración sobre esta solución de la tarea.
#El objetivo de esta solución es que todos puedan comprender los pasos 
#de la solución, sin recurrir a funciones sofisticadas ni preocuparnos por minimizar el tiempo de procesamiento.
#Existen diversas opciones para optimizar este código (por ejemplo lappy()),
#sin embargo, eso lo cubriremos en clase más avanzadas.

#Codigo para obtener los centroides de las manzanas de Mexico

library(foreign)
library(tidyverse)
library(lubridate)
library(readr)
library(sp)
library(rgdal)
library(pracma)
library(aws.s3)
library(R.utils)
library(maptools)
library(rgeos)


################################################################################
#####       Centroides     #####################################################
################################################################################
################################################################################

#Obtenemos los archivos de manzanas de todos los estados de Mexico
setwd("~/Desktop/Datos/Marco_Geoestadistico_Censo_poblacion_y_Vivienda_2020")
vec <- list.files(recursive = TRUE)

#Archivos
archivos <- which(gregexpr(pattern = "m.shp",text = vec) > 0)
quitar1 <- which(gregexpr(pattern = "fm.shp",text = vec) > 0)
quitar2 <- which(gregexpr(pattern = "pem.shp",text = vec) > 0)

#Archivos de interes sobre manzanas
vec <- vec[setdiff(setdiff(archivos,quitar1),quitar2)]

#Hacemos un ciclo sobre todos los archivos y vamos tanto, almacenando un archivo por cada uno
#y tambienhacemos un gran archivo de todos los estados y los centroides de sus manzanas

#Leemos la info del primer estado antes de almacenar
i <- 1
shape=readOGR(vec[i], layer=substr(vec[i],nchar(vec[i])-6,nchar(vec[i])-4))
shape@proj4string

#Obtenemos los centroides
centr <- gCentroid(shape, byid = TRUE)

#Le cambiamos las coordenadas a los centroides para ponerlas en lat,lon (y luego a UTM)
x<-centr@coords[,1]#longitud
y<-centr@coords[,2]#latitud

#Agrupamos las coordenadas a convertir en un data.frame
d <- data.frame(lon=x, lat=y)
coordinates(d) <- c("lon", "lat")

#Escribimos la proyeccion actual de nuestro sistema de coordenadas geográficas
proj4string(d) <- CRS("+proj=lcc +lat_0=12 +lon_0=-102 +lat_1=17.5 +lat_2=29.5 +x_0=2500000 +y_0=0
+ellps=GRS80 +units=m +no_defs ") #proj de centr

#En CRS.new escribimos la proyección a la que queremos convertir nuestros datos
CRS.new <- CRS("+proj=longlat +datum=WGS84 +no_defs")

#Transformamos el sistema
d_proyectado <- spTransform(d, CRS.new)

#Y convertirmos los resultados es un nuevo data.frame
D<-data.frame(d_proyectado)#conviértelo a data frame para que los veas como tabla normal

#Nombre del estado
nombre <- substr(vec[i],4,gregexpr(pattern = "/",vec[i])[[1]][1] - 1)

#A matriz donde acumulamos
A <- cbind(shape@data,D)
A$estado <- nombre


#Acumulamos todos los datos de 2 al 32
for (i in 2:32){
  
  shape=readOGR(vec[i], layer=substr(vec[i],nchar(vec[i])-6,nchar(vec[i])-4))
  shape@proj4string
  
  #Obtenemos los centroides
  centr <- gCentroid(shape, byid = TRUE)
  
  #Le cambiamos las coordenadas a los centroides para ponerlas en lat,lon (y luego a UTM)
  x<-centr@coords[,1]#longitud
  y<-centr@coords[,2]#latitud
  
  #Agrupamos las coordenadas a convertir en un data.frame
  d <- data.frame(lon=x, lat=y)
  coordinates(d) <- c("lon", "lat")
  
  #Escribimos la proyeccion actual de nuestro sistema de coordenadas geográficas
  proj4string(d) <- CRS("+proj=lcc +lat_0=12 +lon_0=-102 +lat_1=17.5 +lat_2=29.5 +x_0=2500000 +y_0=0
+ellps=GRS80 +units=m +no_defs ") #proj de centr
  
  #En CRS.new escribimos la proyección a la que queremos convertir nuestros datos
  CRS.new <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  
  #Transformamos el sistema
  d_proyectado <- spTransform(d, CRS.new)
  
  #Y convertirmos los resultados es un nuevo data.frame
  D<-data.frame(d_proyectado)#conviértelo a data frame para que los veas como tabla normal
  
  #Nombre del estado
  nombre <- substr(vec[i],4,gregexpr(pattern = "/",vec[i])[[1]][1] - 1)
  
  #A matriz donde acumulamos
  AUX <- cbind(shape@data,D)
  AUX$estado <- nombre
  
  #Acumulamos en la matriz de todos los estados
  A <- rbind(A,AUX)
  print(i)
}

#Guardamos los datos de los centroides de todas las manzanas de Mexico
setwd("~/Desktop/Datos/Centroides_NSE/results")
write_csv(A,"Centroides_MX.csv")