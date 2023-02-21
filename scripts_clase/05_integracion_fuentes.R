#Paquetes a utilizar en la lección 6

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
library(rgeos)

##############################################################################
#### Función Over
#### Alternativa eficiente a los ciclos de inpolygon()


#### Intersección shape-punto

#Cargamos los datos de las carpetas de investigacion

#Cremos una m.a. de 1,000 registros con coordenadas validas
file <- paste0(getwd(), '/datos/Delitos.csv')
A <- read_csv(file) |>
  filter(latitud != 0 & longitud != 0) |>
  sample_n(size = 1000,replace = FALSE)


#Cargamos una shapefile con las alcaldias 
alcaldias <- paste0(getwd(), '/datos/Alcaldias/alcaldias.shp')
shape=readOGR(alcaldias, layer="alcaldias")

#Recordemos el contenido tabular del shapefiles
shape@data %>% View()


#Pregunta:
#¿Cómo podemos determinar en qué delegación caen los puntos sin utilizar la función inpolygon()?
#R: con función OVER()




#Como siempre, construimos el SpatialPointsDataFrame utilizando los datos de los delitos:

#Nota importante:
  #En este caso, los pasos comentados NO son necesarios pues los puntos del shape y delitos están en la 
  #misma proyección. En otro caso, sí debes hacerlo antes de intersectar con over() o inpolygon().
  
# d <- data.frame(lon=A$longitud, lat=A$latitud)
# coordinates(d) <- c("lon", "lat")
# #En proj4string(d) pones la proyección actual
# proj4string(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# CRS.new <-shape@proj4string@projargs
# d.ch1903 <- spTransform(d, CRS.new)
# D<-data.frame(d.ch1903)#conviértelo a data frame para que los veas como tabla normal

#En nuestro caso, tomamos las columnas necesarias de la matriz de delitos para obtener un SpatialPointsDataFrame
dat <- data.frame(Longitude = A$longitud,
                  Latitude = A$latitud,
                  names = 1:nrow(A))

coordinates(dat) <- ~ Longitude + Latitude

#Declaramos la proyección del shape
proj4string(dat) <- proj4string(shape)

#Finalmente utilizamos la función over que recibe 2 arguments:
  #dat: SpatialPointsDataFrame las coordenadas que queremos intersectar
  #shape: shapefile de referencia para asignar etiqueta (alcaldia)
C <- over(dat,shape)

#Observamos el contenido
View(C)

#Pegamos los resultados a la matriz original de delitos
A <- cbind(A,select(C,nomgeo,cve_mun))

#Oservamos los resultados
View(A)





#### Intersección shape-shape

#Tambien es posible realizar la interseccion shape vs shape
  #Sin embargo, es necesario verificar que los conjuntos tienen la misma proyeccion

#Cargamos un shapefile sobre codigos postales en la CDMX
#setwd("~/Desktop/Datos/CP/nuevo_cp_cdmx")
cp_path <- paste0(getwd(), '/datos/cp_cdmx/cp_09cdmx_v8_1.shp')
layer <- ogrListLayers(cp_path)
cp <- readOGR(cp_path, layer=layer)

#Revisamos el contenido
cp@data %>% View()

#Para responder la pregunta:
  #¿En qué alcaldia cae cada código postal de la CDMX?
B <- over(cp,shape)

#Finalmente, conocemos a que alcaldia pertenece cada cp
CP <- cbind(cp@data %>% select(cp),B %>% select(nomgeo,cve_mun))






### ¿Que pasa si intersectamos dos shapefiles con distinta proyeccion?
setwd("~/Downloads/otro_cp")
alc_bis <- readOGR("09mun.shp", layer=ogrListLayers("09mun.shp"))

#Tambien es un shapefiles de alcaldias
plot(alc_bis)

#Revisamos la proyeccion
#No esta en lat,lon
alc_bis@proj4string


#Utilicemos el shapefile sobre codigos postales
cp@proj4string

#Funcion over
over(cp,alc_bis)

# Error in overGeomGeom(x, y, returnList = TRUE, minDimension = minDimension) : 
#   identicalCRS(x, y) is not TRUE









#Si queremos asignar el cp de cada delito, tenemos que repetir los pasos del SpatialDataFrame

#Utilizamos la misma matriz dat ya creada

# dat <- data.frame(Longitude = A$longitud,
#                   Latitude = A$latitud,
#                   names = 1:nrow(A))
# 
# coordinates(dat) <- ~ Longitude + Latitude

#Declaramos la proyección del shape
  #como shape tiene la misma proyeccion que cp, podríamos usar proj4string(dat) <- proj4string(shape)
  #pero siempre es buena practica irlo modificando
proj4string(dat) <- proj4string(cp)

#Finalmente utilizamos la función over que recibe 2 arguments:
D <- over(dat,cp) %>% select(cp)

#Observamos el contenido
View(D)

#Pegamos los resultados a la matriz original de delitos con el cp

#Asignamos de forma eficiente el cp y alcaldia a cada delito
A <- cbind(A,D)





