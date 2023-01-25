#######################################################################################
#######################################################################################
################      CLASE 3                     #####################################
################   MCD Carlos Castro Correa          ##################################
#######################################################################################
#######################################################################################

#Paquetes  a utilizar en la lección 3
library(foreign)
library(tidyverse)
library(lubridate)
library(readr)
library(sp)
library(rgdal)
library(pracma)
library(aws.s3)
library(R.utils)
library(geosphere)

#Cambio el directorio
#setwd("~/Downloads/Datos_Espaciales/Alcaldias")


#Para los ejemplos de esta clase necesitamos crear un conjunto de puntos en coordenadas geográficas y otro en UTM Mercator

#Leemos el shapefile de alcaldias de la clase 2
shape=readOGR("./datos/alcaldias/alcaldias.shp", layer="alcaldias")

#Matriz de coordenadas geográficas con latitud y longitud
coords <- data.frame(longitud = shape@polygons[[1]]@Polygons[[1]]@coords[,1],latitud = shape@polygons[[1]]@Polygons[[1]]@coords[,2])

#Matriz de puntos en UTM Mercator
  #Utilizamos las coordenadas geograficas y las proyectamos
d <- data.frame(lon=coords$longitud, lat=coords$latitud)
coordinates(d) <- c("lon", "lat")

#Estan en lat,lon entonces declaramos la proyeccion usual
sputm <- SpatialPoints(d, proj4string=CRS("+proj=longlat +datum=WGS84"))

#Ya sabemos que todos los puntos estan en la CDMX y por lo tanto, pertenecen a la UTM Zone 14
proyeccion<-CRS("+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs ") 

#Transformamos los datos
UTM <- spTransform(sputm, proyeccion) %>% data.frame()
colnames(UTM) <- c("lon_UTM","lat_UTM")





########################################################################
########################################################################
#Distancias

#Distancia entre 2 puntos en coordenadas geográficas 
#distHaversine(punto1, punto2, r=6378137)
distHaversine(c(coords$longitud[1900],coords$latitud[1900]),c(coords$longitud[1],coords$latitud[1]), r=6378137)

#Esta fórmula nos entrega la distancia en metros:
#1119.646

#Punto 1
c(coords$longitud[1900],coords$latitud[1900])
#Punto 2
c(coords$longitud[1],coords$latitud[1])

#Puedes comprobar este resultado comparando con Google Maps
#https://www.google.com.mx/maps/dir/19.39475,-99.18871++/19.40356,-99.19384/@19.3991982,-99.1945889,16z/data=!3m1!4b1!4m7!4m6!1m3!2m2!1d-99.18871!2d19.39475!1m0!3e0


#Distancia Euclidiana
#Definimos una funcion auxiliar
euclidean <- function(a, b) sqrt(sum((a - b)^2))

#Tomamos dos puntos cualesquiera de la matriz de UTM Mercator
pto1 <- c(UTM$lon_UTM[150],UTM$lat_UTM[150])
pto2 <- c(UTM$lon_UTM[19],UTM$lat_UTM[19])

euclidean(pto1,pto2)



#Comparación entre Distancia de Haversine y Euclidiana

#Consideramos el mismo par de puntos, con la proyección y distancia respectiva
#Puntos 1900 y 1

#Haversine
distHaversine(c(coords$longitud[1900],coords$latitud[1900]),c(coords$longitud[1],coords$latitud[1]), r=6378137)


pto1 <- c(UTM$lon_UTM[1],UTM$lat_UTM[1])
pto2 <- c(UTM$lon_UTM[1900],UTM$lat_UTM[1900])
#Euclidiana
euclidean(pto1,pto2) #Hay una diferencia de unos cuentos metros








########################################################################
########################################################################
#Cálculos comunes

#Distancia promedio (Qué tan lejos estamos de los demás puntos del conjunto)
  #Entre un punto de referencia y todos los otros puntos del conjunto
  #Consideremos al punto 100 en Coordenadas, #¿Cuál es la distancia promedio a los demás puntos?


#Nota: hay muchas otras auxiliares más eficientes que utilizar for() o while()
#sin embargo, utilizaremos estas funciones con fines ilustrativos

mat <- matrix(0,nrow(coords),1)

for(i in 1:nrow(mat)){
  mat[i,1] <- distHaversine(c(coords$longitud[100],coords$latitud[100]),c(coords$longitud[i],coords$latitud[i]), r=6378137)
}

#¿Cuál es la distancia promedio a los demás puntos?
#Nota: recuerda excluir al punto mismo o estaria subestimando la respuesta real
mean(mat[c(1:99,101:nrow(mat)),1])

#Si hubieras considerado la distancia al punto mismo (cero), el resultado seria:
mean(mat[,1]) #Parece insignificante pero siempre hay que considerarlo



########################################################################
########################################################################
#Ejercicios

#Considerando los puntos de una matriz de coordenadas geográficas COORDS 
#y dado un punto de referencia P = (lat,lon), programa lo siguiente en R:


#1. Una funcion para obtener el punto del conjunto COORDS que esté más cercano a P y la distancia entre estos

#2. Una funcion para obtener el punto del conjunto COORDS que esté más lejano a P y la distancia entre estos

#3. Una funcion para obtener todos los punto del conjunto COORDS que estén a menos de r metros de P y la distancia entre estos puntos y P. 
#(Puedes dar la respuesta con una matriz de 2 columnas = [punto de COORDS,distancia a P])

#4. ¿Cambiaría alguno de los códigos que programaste si fueran puntos en UTM Mercator (además de la fórmula de distancia)?
#Piensa en términos geométricos.












########################################################################
########################################################################
#Marco Geoestadístico Nacional

#Descarga los datos para Chihuahua: https://www.dropbox.com/s/vt477y1c2aidn7l/08_chihuahua.zip?dl=1
#setwd("~/Downloads/08_chihuahua/conjunto_de_datos")


#Cargamos los datos sobre las manzanas de Chihuahua
shape=readOGR("./datos/08_chihuahua/conjunto_de_datos/08m.shp", layer="08m")

#El shapefile tiene informacion sobre 111,956 manzanas
length(shape)

#Tipos de variables
str(shape@data)

#Resumen estadistico de los datos
  #¿Que significa cada variable? Asegurate de leer los diccionarios de datos antes de utilizar los datos
  #https://www.inegi.org.mx/temas/mg/#Descargas
summary(shape@data)

shape@polygons[[1]]@Polygons[[1]]@coords[,1]

#Revisamos la proyeccion
shape@proj4string



########################################################################
#Matriz de puntos en la proyeccion del INEGI a coordenadas geográficas
d <- data.frame(lon=shape@polygons[[1]]@Polygons[[1]]@coords[,1], lat=shape@polygons[[1]]@Polygons[[1]]@coords[,2])
coordinates(d) <- c("lon", "lat")

#Estan en la proyeccion que publica el INEGI
sputm <- SpatialPoints(d, proj4string=CRS("+proj=lcc +lat_0=12 +lon_0=-102 +lat_1=17.5 +lat_2=29.5 +x_0=2500000 +y_0=0 +ellps=GRS80 +units=m
+no_defs "))

#Las convertimos a lat,lon
proyeccion<-CRS("+proj=longlat +datum=WGS84 ") 

#Transformamos los datos
matriz <- spTransform(sputm, proyeccion) %>% data.frame()
colnames(matriz) <- c("longitud","latitud")

#Centroide de la manzana 1 de Chihuahua
c(mean(matriz$latitud),mean(matriz$longitud))

#Podemos observar la manzana
plot(matriz)

lines(matriz)
















########################################################################
########################################################################
#Microdatos

setwd("~/Downloads")

#Microdatos a nivel manzana sobre Chihuahua
micro <- read_csv("RESAGEBURB_08CSV20.csv")

View(micro)

#Esta matriz tiene muchas columnas, sin embargo, por ahora nos quedaremos solo con las primeras
micro <- select(micro, ENTIDAD:POBTOT)

#¿Que adaptaciones necesitas para cruzar esta matriz con los datos de shape@data?






########################################################################
#AWS S3
#La funciones de este paquete están en https://cran.r-project.org/web/packages/aws.s3/aws.s3.pdf

#Credenciales

#Accede a las credenciales de tu cuenta
#https://docs.aws.amazon.com/powershell/latest/userguide/pstools-appendix-sign-up.html

Sys.setenv("AWS_ACCESS_KEY_ID" =  "",
           "AWS_SECRET_ACCESS_KEY" = "",
           "AWS_DEFAULT_REGION" = "us-east-1") #Asurate de estar en la zona que creaste el bucket

#Lista de todos los buckets
bucketlist() #Te entrega todos los buckets que hay en tu cuenta, en esta zona

#Archivos que tengo creados en el bucket 
archivos<-get_bucket_df("testitam",max = 100000)

#Puedo subir, descargar, editar, etc un archivo a AWS desde mi computadora en R
#Referencias: https://cran.r-project.org/web/packages/aws.s3/aws.s3.pdf






