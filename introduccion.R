#######################################################################################
#######################################################################################
################      CLASE 2                     #####################################
################MCD Carlos Castro Correa          #####################################
#######################################################################################
#######################################################################################

#Paquetes  a utilizar en la lección 2
library(foreign)
library(tidyverse)
library(lubridate)
library(readr)
library(sp)
library(rgdal)
library(pracma)
library(aws.s3)
library(R.utils)

#Descarga los archivos espaciales y colocalos en una carpeta de tu computadora
#Liga al shapefile: https://www.dropbox.com/sh/auep5kzqt4x0o1b/AABm4OxGu9jBd5oiG3rPZF71a?dl=0

#Cambio el directorio
setwd("~/Downloads/Datos_Espaciales")


#######################################################################################
#############          COMPONENTES DE UN SHAPEFILE      ###############################
#######################################################################################

#Leemos los archivos del shapefile en el conjunto Alcaldias
setwd("~/Downloads/Datos_Espaciales/Alcaldias")

#4 archivos que componen el shapefile
list.files()

########################################################################
#DBF: parte tabular
#Analizamos el .dbf
edos <- read.dbf("alcaldias.dbf") %>% data.frame()
View(edos)

#Revisamos el tipo de datos de cada variable de la base
str(edos)

#Analizamos el resumen sobre las variables
summary(edos)


########################################################################
########################################################################
#Puedes leer la tabla asociada al shapefiles y hacer operaciones con tidyverse

#Numero promedio de radios por alcaldia
med <- select(edos,radio) %>% unlist() %>% mean()
med

#Estados con más radios que la media por alcaldua
filter(edos,radio > med) %>% select(nomgeo)

#Numero de autos de la alcaldia con mas autos
select(edos,radio) %>% unlist() %>% max()

#Numero de lavadoras de la alcaldia con menos lavadora
select(edos,lavadora) %>% unlist() %>% min()


########################################################################
#SHP
#Esta instrucción nos permite ver todas las capas que tiene nuestro archivo (hay shapefiles que tienen más de una capa)
ogrListLayers("alcaldias.shp")

#En nuestro caso, solo tenemos una capa llamada "estados"
# [1] "alcaldias"
# attr(,"driver")
# [1] "ESRI Shapefile"
# attr(,"nlayers")
# [1] 1

#Cargamos el shapefile en R
#La funcion recibe 2 atributos: el nombre del .shp que queremos llamar y el nombre de la capa (comando ogrListLayers)
shape=readOGR("alcaldias.shp", layer="alcaldias")

#El archivo contiene información sobre 32 registros y 2 variables
# with 16 features - son 16 alcaldias
# It has 9 fields - las variables asociadas

#Podemos visualizar el shape desde R con una instruccion basica
par(mar=c(0,0,0,0))
plot(shape, col="#f2f2f2", bg="skyblue", lwd=0.25, border=0)


##################################################################
#Shapefile data
#Tambien podemos ver la base de datos sin tener que cargar el dBase (dbf)
shape@data %>% View()

#Si cargamos el DBF, se observa la misma información
edos %>% View()

#Podemos ver todos los elementos que tiene el shp desde la consola:
shape

#Para analizar los distintos atributos podemos utilizar "@" y TAB
shape@data

shape@polygons#Informacion sobre los poligonos que componene al shape

#Tamaño del shape - podemos utilizar el siguiente comando:
shape %>% length()

#Observamos que el shape tiene información sobre 16 pológonos distintos
# > shape %>% length()
# [1] 16

#Y esto coincide con el numero de renglones en la BD del shapefile
shape@data %>% dim()

#Nota: ""Cada poligono tiene asociadas un grupo de variables desde la BD""

#Ademas tiene otros atributos interesantes:

shape@plotOrder 

#bbox nos da resumen rápido sobre la distribución de los puntos en el espacio
shape@bbox #Limites del shapefile

# --------  Explicacion gráfica
#Origen bbox











########################################################################
#Poligonos y coordenadas

#shape %>% length()

#Accedemos a la informacion del primer poligono (1 de 16)

shape@polygons[[1]] #¿Qué elementos contiene?

#Cada poligono esta compuesto de otros subpoligonos tales que representan la forma de un objeto espacial
#Sin embargo, en general, cada poligono se compone de 1 subpoligono (A excepción de Islas)

shape@polygons[[1]]@Polygons[[1]]

#Podemos conocer el area del subpoligono 1 del poligono 1
shape@polygons[[1]]@Polygons[[1]]@area

#Lo más importante son las coordenadas
shape@polygons[[1]]@Polygons[[1]]@coords #Este conjunto tiene todas las matriz de puntos que forman el poligono 1

#Latitud
shape@polygons[[1]]@Polygons[[1]]@coords[,2]

#Longitud
shape@polygons[[1]]@Polygons[[1]]@coords[,1]

#A este par de puntos en lat,lon se le conoce como Coordenadas Geográficas ***


#Podemos graficar este conjunto de coordenadas
plot(shape@polygons[[1]]@Polygons[[1]]@coords[,1],shape@polygons[[1]]@Polygons[[1]]@coords[,2])

#¿Cual es la forma de la alcaldia Alvaro Obregon?,busca en google la forma

#¿Que puedes deducir sobre esta representación gráfica? ¿Qué pasa con los puntos?
plot(shape@polygons[[1]]@Polygons[[1]]@coords[,1],shape@polygons[[1]]@Polygons[[1]]@coords[,2])



# --------  Explicacion gráfica
#Graficas y lineas para el shape


#Visualizamos la cuarta alcaldia:
par(mar=c(0,0,0,0))
aux <- shape@polygons[[4]]@Polygons[[1]]@coords %>% data.frame()
colnames(aux) <- c("lon","lat")
#Puntos el shape
plot(aux, , bg="blue", lwd=0.25)

#Unimos y hace sentido con la forma "real" del estado
lines(aux)

#Nota: solo los lineas y puntos...









#Notas
########################################################################
#Proyeccion ***

#¿Qué es una proyección cartográfica y cómo se originan?

#Los puntos expresados en coordenadas geográficas de lat, lon se expresan de la siguiente forma
#"Deprecated Proj.4 representation: +proj=longlat +datum=WGS84 +no_defs"

#Conocer y saber manipular la proyección de nuestros datos es FUNDAMENTAL para un correcto análisis
#Siempre puedes conocer en que proyección están tus puntos con la siguiente instrucción:
shape@proj4string

#Este shapefile esta en coordenadas geográficas

#Esta es el CRS (Coordinate Reference System) de las coordenas geográficas de latitud y longitud
#Como revisamos, estos datos vienen generalmente incluidos en todos los archivos espaciales
#KML/KML, SHP y GEOJSON. Lo puedes determinar con la misma instruccion del paquete rgdal


#Proyección de coordenadas
  #¿Cómo podrías proytectar en R?



#############   CAMBIO DEL SISTEMA DE COORDENADAS          #################################################

#Supongamos que necesitamos cambiar las coordenadas geográficas a un sistema con coordenadas proyectadas
#Queremos proyectar nuestros puntos en este sistema:
#1. +proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=12 +lon_0=-102 +x_0=2500000 +y_0=0 +ellps=GRS80 +units=m +no_defs

#A continuación, se describe el proceso paso a paso para hacer la conversión, a partir de las coordenadas
#Sin importar si es un KML/KML, SHP y GEOJSON, ya sabemos procesar los archivos para agrupar las coordenadas
#En este caso, partimos del shape de Alcaldias que acabamos de leer

x<-shape@polygons[[1]]@Polygons[[1]]@coords[,1]#longitud
y<-shape@polygons[[1]]@Polygons[[1]]@coords[,2]#latitud

#Agrupamos las coordenadas a convertir en un data.frame
d <- data.frame(lon=x, lat=y)
coordinates(d) <- c("lon", "lat")

#Escribimos la proyeccion actual de nuestro sistema de coordenadas geográficas
proj4string(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs ") # lat,lon

#En CRS.new escribimos la proyección a la que queremos convertir nuestros datos
CRS.new <- CRS("+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=12 +lon_0=-102 +x_0=2500000 +y_0=0 +ellps=GRS80 +units=m +no_defs")

#Transformamos el sistema
d_proyectado <- spTransform(d, CRS.new)

#Y convertirmos los resultados es un nuevo data.frame
D<-data.frame(d_proyectado)#conviértelo a data frame para que los veas como tabla normal

#Nota que cada uno de los conjuntos anteriores son de distinta clase:
class(d_proyectado) #Tipo "SpatialPoints"

class(D)



#############   CAMBIO a UTM         #################################################

#Primero necesitamos conocer la zona UTM o las zonas en donde están nuestros puntos
#Los puntos de la CDMX están en la zona 14, vamos a utilizar el polígono de la Álvaro Obregon
UTM <- '14'
x<-shape@polygons[[1]]@Polygons[[1]]@coords[,1]#longitud
y<-shape@polygons[[1]]@Polygons[[1]]@coords[,2]#latitud

#Repetimos los pasos anteriores
d <- data.frame(lon=x, lat=y)
coordinates(d) <- c("lon", "lat")

#Estan en lat,lon entonces declaramos la proyeccion usual
sputm <- SpatialPoints(d, proj4string=CRS("+proj=longlat +datum=WGS84"))

#Declaramos la nueva proyeccion con la zona UTM a donde pertenecen los puntos
#Si tienes puntos de distintas zonas, deberas hacer el proceso por separado y despues unificar
proyeccion<-CRS("+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs ") 

#Transformamos los datos
spgeo <- spTransform(sputm, proyeccion)
spgeo<-as.data.frame(spgeo)
colnames(spgeo) <- c("lon_UTM","lat_UTM")#Tenemos un equivalente como "lat,lon" en coordendas en el plano cartesiano



#¿Que diferencias puedes observar entre estos tipos de coordenadas?
cbind(spgeo[1:10,],d[1:10,]) %>% cbind(D[1:10,]) %>% View()

  #Las diferencias se explican por la forma de proyectar y la superficie de referencia






########################################################################
#Función inpolygon()

x<-shape@polygons[[1]]@Polygons[[1]]@coords[,1]#longitud
y<-shape@polygons[[1]]@Polygons[[1]]@coords[,2]#latitud

#Alcaldia Alvaro Obregon
D <- data.frame(lat = y, lon = x)

##########Nota: el centroide de la delegación AO se calcula como:#
c(mean(D$lat),mean(D$lon))


####Pregunta: ¿El ITAM está en la alcaldía Álvaro Obregón?

#Coordenadas del ITAM Río Hondo
itam <- data.frame(lat = 19.344407538815684, lon = -99.20002940738932)

inpolygon(itam$lat, itam$lon, D$lat, D$lon, boundary = FALSE)



####Pregunta: ¿El estadio Omnilife está en la Alcaldía AO?
omni <- data.frame(lat = 20.681776386193583 , lon = -103.4626562316925)

inpolygon(omni$lat, omni$lon, D$lat, D$lon, boundary = FALSE)







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






