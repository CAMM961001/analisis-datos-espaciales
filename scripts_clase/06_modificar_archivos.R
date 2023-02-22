#######################################################################################
#######################################################################################
################      CLASE 7                     #####################################
################  MCD Carlos Castro Correa        #####################################
#######################################################################################
#######################################################################################

#Paquetes a utilizar en la lección 7

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
library(foreign)
#Instalamos este paquete nuevo
library(rvest)

###### Modificacion de shapefile - agregar o eliminar variables

#Leemos los datos de alcaldias y cp de la clase 6

########################################################################
#BD Alcaldias
shape=readOGR("~/Downloads/Datos_Espaciales/Alcaldias/alcaldias.shp", layer="alcaldias")

#Recordemos el contenido tabular del shapefiles
shape@data %>% View()

#Vemos el contenido
plot(shape)



########################################################################
#BD Codigo postal
setwd("~/Desktop/Datos/CP/nuevo_cp_cdmx")
cp <- readOGR("cp_09cdmx_v8_1.shp", layer=ogrListLayers("cp_09cdmx_v8_1.shp"))

#Revisamos el contenido
cp@data %>% View()




######Pregunta:
#¿Cuantos códigos postales tienen cada alcaldia de la CDMX?

#Antes de cruzar siempre revisamos la proyección de cada shapefile
cp@proj4string

shape@proj4string

#Es la misma entonces podemos utilizar OVER()


#Podemos hacer una interseccion espacial shape-shape
  #¿En qué alcaldia cae cada código postal de la CDMX?
A <- over(cp,shape)

#Agregamos los datos y los guardamos en un data.frame
alc_cp <- table(A$nomgeo) %>% data.frame()
colnames(alc_cp) <- c("nomgeo","cp_totales")


#Que pasa si hubieramos modificado el orden de los argumentos OVER:
    #¿Cómo interpretas este resultado? ¿?¿?¿?¿?¿?
over(shape,cp) %>% View()



#################################################################
#################################################################
#####    Modificación de variables      #########################
#################################################################

#Existen dos formas para acceder a datos tabulares de un shape
#1.Directamente del shape
shape@data %>% View()

#2.La otra es accediendo al dbf con el paquete foreign
setwd("~/Downloads/Datos_Espaciales/Alcaldias")
alcaldias <- read.dbf("alcaldias.dbf")

#En nuestro caso vamos a utilizar la segunda

#Consideremos el data.frame del numero total de cp por cada alcaldias
View(alc_cp)

#Cruzamos con los datos tabulares del shapefile de alcaldias

alcaldias <- left_join(x = alcaldias, y = alc_cp, by = 'nomgeo')

#¿Como hacemos un nuevo shapefile con estos datos tabulares?
  
#Tenemos 3 opciones: proceso manual o automatizando con código (2)

#1- Opcion manual
  #Primero guardamos el nuevo dbf
  #En mi compu creo otro directorio que tiene el mismo shape pero me sirve para las pruebas
    
    setwd("~/Downloads/Datos_Espaciales/Alcaldias_test") 
    write.dbf(alcaldias,"alcaldias_bis.dbf")
    
    #Ahora accedo manualmente al folder y cambio el nombre de los demas archivos
      #... ir a la carpeta
    

#Una vez que modifique los nombres puedo volver a cargar el shapefile y los datos ya estarán actualizados

shape_bis=readOGR("~/Downloads/Datos_Espaciales/Alcaldias_test/alcaldias_bis.shp", layer="alcaldias_bis")
    
shape_bis@data %>% View()

#Por supuesto el nuevo dbf tambien tiene variables modificadas:

nuevo_dbf <- read.dbf("alcaldias_bis.dbf")
    
     #2-Tambien lo podriamos sobreescribir con el mismo nombre para ahorrarnos el paso anterior (aunque puede ser desordenado)
    write.dbf(alcaldias,"alcaldias_bis.dbf")
    
    
#Ahora podriamos visualizar estas variables con el mapa de Leaflet()

m <- leaflet(shape_bis) %>%
  addPolygons() %>% 
  addTiles() %>% addProviderTiles(providers$CartoDB.VoyagerLabelsUnder)
m

#Creamos la paleta de colores por la variable de interes
pal <- colorNumeric(palette = "Blues",domain =  nuevo_dbf$cp_totales)

#Muestra el numero de televisiones por estado, entre más azul, tenemos mas televisiones
m %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
              color = ~pal(cp_totales %>% as.numeric()),popup = ~as.character(nomgeo))







########################################################################
########################################################################
#######             Web Scrapping                      #################
########################################################################
########################################################################

#Para este ejercicio utilizaremos los datos del Portal Transfermarket https://www.transfermarkt.es/

  #Exploremos el portal...

library(rvest)

#Observamos los datos de Chivas

  #https://www.transfermarkt.es/deportivo-guadalajara/startseite/verein/6711

  #2023
    # https://www.transfermarkt.es/cd-guadalajara/startseite/verein/6711?saison_id=2023

  #2007
    # https://www.transfermarkt.es/cd-guadalajara/startseite/verein/6711?saison_id=2007


#Ahora veamos los datos sobre Rayados de Monterrey
  
  #2022
    #https://www.transfermarkt.es/cf-monterrey/startseite/verein/2407/saison_id/2022



###########################################
###########################################
#Aqui podemos encontrar a todos los equipos
  #https://www.transfermarkt.es/liga-mx-clausura/startseite/wettbewerb/MEX1 


#Creamos una tabla con todos los jugadores, valor y lugar de nacimiento para la temporada 2022
#De Chivas y Cruz Azul

#Acumulado por equipos
A <- data.frame(matrix(0,0,5))
colnames(A) <- c("valor","nacimiento","nombre","team","temporada")

#Lista de todos los equipos y sus claves
nombres <- c('deportivo-guadalajara','cd-cruz-azul')
codigo_equipo <- c('6711','3711') #Tambien se obtienen del mismo URL
annus <- 2022


#Selecciona el equipo
i <- 1


###########################################
###########################################
#HMTL
#Es posible ver el html de cualquier página


j <- 1
#hot100page <- 'https://www.transfermarkt.es/atlas-guadalajara/kader/verein/8590/plus/0/galerie/0?saison_id=2018'
hot100page <- paste('https://www.transfermarkt.es/',nombres[j],'/kader/verein/',codigo_equipo[j],'/plus/0/galerie/0?saison_id=',annus[i],sep='')

hot100 <- read_html(hot100page)
hot100
str(hot100)

#Seleccionamos el body
body_nodes <- hot100 %>% 
  html_node('body') %>% 
  html_children()
body_nodes 

#Seleccionamos los children
body_nodes %>% 
  html_children()
body_nodes



###############################################################################################
###############################################################################################
###############################################################################################
#Fecha de contrato
nacimiento <- hot100 %>% 
  rvest::html_nodes('body') %>% 
  xml2::xml_find_all("//td[contains(@class, 'zentriert')]") %>% 
  rvest::html_text()

nacimiento

#Nos quedamos solo con las fechas
nacimiento <- nacimiento[which(nchar(nacimiento) > 9)] %>% substr(1,10)

nacimiento

###############################################################################################
###############################################################################################
###############################################################################################

#Valor
valor <- hot100 %>% 
  rvest::html_nodes('body') %>% 
  xml2::xml_find_all("//td[contains(@class, 'rechts hauptlink')]") %>% 
  rvest::html_text()

valor

###############################################################################################
###############################################################################################
###############################################################################################

#Algunos campos no son tan directos y hay 2 opciones:
  #1. Entender perfectamente la estructura y funcionamiento de los html para identificar y extraer elementos mas detallados
  
  #2. Utilizar expresiones regulares para acceder a la información


#Pais 
pais <- hot100 %>% 
  rvest::html_nodes('body') %>% 
  xml2::xml_find_all("//td[contains(@class, 'zentriert')]")

pais

#¿Como podemos identificar el pais?
#Podemos regresar al código de la página

pais[[3]]
#¿Que tiene en común estas lineas? ¿Como podemos "aislar el nombre del pais"?


pais[[7]]


pais[[11]]


#title
#Para extraerlo podemos utilizar expresiones regulares
  #Podemos utilizar la funcion gregexpr()

aux <- gregexpr(pattern = "title",text = pais[[3]])
aux
aux[[1]][1]


substr(x = pais[[3]],start = aux[[1]][1],aux[[1]][1]+16)

substr(x = pais[[3]],start = aux[[1]][1],aux[[1]][1]+14)

#¿Como podemos "aislar el nombre del pais"?

