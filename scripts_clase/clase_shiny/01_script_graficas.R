
library(ggplot2)

###################################33 Gráfica de barras

df <- data.frame(tipo=c("tipo1", "tipo2", "tipo3"),
                 valor=c(4.2, 10, 29.5))
head(df)

## Básico

ggplot(data=df, aes(x=tipo, y=valor)) + ## parametros básicos
  geom_bar(stat="identity")

## Colorear barras

ggplot(data=df, aes(x=tipo, y=valor)) + ## parametros básicos
  geom_bar(stat="identity", fill="blue") ## selección de color

## Colorear barras por tipo

ggplot(data=df, aes(x=tipo, y=valor, fill=tipo)) + ## parametros básicos
  geom_bar(stat="identity") ## selección de color

## Formato

ggplot(data=df, aes(x=tipo, y=valor, fill=tipo)) + ## parametros básicos
  geom_bar(stat="identity") +
  ggtitle("Gráfica de barras") +
  xlab("Título eje X") +
  ylab("Título eje Y") +
  theme(plot.title = element_text(hjust = 0.9,color = "black", size =15), ##Formato de título
        axis.title.y=element_text(hjust = 0.5,color = "red", size =13), ##Formato de título eje y
        axis.title.x = element_text(color = "black"),##Formato de título eje x
        axis.text.x=element_text(colour="green", size =11), ##Formato de texto eje x
        axis.text.y=element_blank() ##Formato de texto eje y
      
  )



################################### Gráfica de barras comparación

df2 <- data.frame(region=rep(c("Region1", "Region2"), each=3),
                  tipo=rep(c("Tipo1", "Tipo2", "Tipo3"),2),
                  len=c(6.8, 15, 33, 4.2, 10, 29.5))
head(df2)

# Barras apiladas
ggplot(data=df2, aes(x=tipo, y=len, fill=region)) +
  geom_bar(stat="identity")


# Barras lado a lado
ggplot(data=df2, aes(x=tipo, y=len, fill=region)) +
  geom_bar(stat="identity", position=position_dodge()) ## Aquí cambio la posición del apilamiento


ggplot(data=df2, aes(x=tipo, y=len, fill=region)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("Gráfica de barras apilada") +
  xlab("Título eje X") +
  ylab("Título eje Y") +
  theme(plot.title = element_text(hjust = 0.9,color = "black", size =15), ##Formato de título
        axis.title.y=element_text(hjust = 0.5,color = "red", size =13), ##Formato de título eje y
        axis.title.x = element_text(color = "black"),##Formato de título eje x
        axis.text.x=element_text(colour="green", size =11), ##Formato de texto eje x
        axis.text.y=element_blank() ##Formato de texto eje y
        
  )



################################### Scatter plot

df <- mtcars[, c("mpg", "cyl", "wt", "qsec", "vs")]
df$cyl <- as.factor(df$cyl) # convierto el número de cilindros en una categoróa
head(df)

## Dataframe de carros con las variables
# mpg Miles/(US) gallon
# cyl Number of cylinders
# wt Weight (lb/1000)
# qsec 1/4 mile time
# vs V/S


# scatter plot básico
ggplot(df, aes(x=wt, y=mpg)) + ## Datos y columnas a usar
  geom_point()

# Formato de los puntos
ggplot(df, aes(x=wt, y=mpg)) +
  geom_point(size=2, shape=23) ## Tamaño y tipo de forma

# Tamaño del punto condicionado a una variable
ggplot(df, aes(x=wt, y=mpg)) + 
  geom_point(aes(size=qsec)) 

# Etiqueta en cada punto
ggplot(df, aes(x=wt, y=mpg)) + 
  geom_point()  + 
  geom_text(label=rownames(mtcars)) ## selecciono la columna que usaré de etiqueta

## Coloreo por categoría
ggplot(df, aes(x=wt, y=mpg, color=cyl)) + 
  geom_point()  + 
  geom_text(label=rownames(mtcars)) 


## Formato
ggplot(df, aes(x=wt, y=mpg, color=cyl)) + 
  geom_point()  + 
  geom_text(label=rownames(mtcars)) +
  theme_bw() +                          ## Con esto cambio el tema principal (fondo y colores)
  ggtitle("Scatter plot") +
  xlab("Título eje X") +
  ylab("Título eje Y") +
  theme(plot.title = element_text(hjust = 0.9,color = "black", size =15), ##Formato de título
        axis.title.y=element_text(hjust = 0.5,color = "red", size =13), ##Formato de título eje y
        axis.title.x = element_text(color = "black"),##Formato de título eje x
        axis.text.x=element_text(colour="green", size =11), ##Formato de texto eje x
        axis.text.y=element_blank() ##Formato de texto eje y
        
  )


################################### Líneas

df <- data.frame(tipo=c("tipo1", "tipo2", "tipo3"),
                 valor=c(4.2, 10, 29.5))
head(df)

# Línea básica
ggplot(data=df, aes(x=tipo, y=valor, group=1)) +
  geom_line()+
  geom_point()

# Tipo de línea
ggplot(data=df, aes(x=tipo, y=valor, group=1)) +
  geom_line(linetype = "dashed")+
  geom_point()

# Color de línea y punto
ggplot(data=df, aes(x=tipo, y=valor, group=1)) +
  geom_line(color="red")+
  geom_point(color="blue")
# Formato

ggplot(data=df, aes(x=tipo, y=valor, group=1)) +
  geom_line(color="red",linetype = "dashed")+
  geom_point(color="blue")+
  theme_bw() +                          ## Con esto cambio el tema principal (fondo y colores)
  ggtitle("Línea") +
  xlab("Título eje X") +
  ylab("Título eje Y") +
  theme(plot.title = element_text(hjust = 0.9,color = "black", size =15), ##Formato de título
        axis.title.y=element_text(hjust = 0.5,color = "red", size =13), ##Formato de título eje y
        axis.title.x = element_text(color = "black"),##Formato de título eje x
        axis.text.x=element_text(colour="green", size =11), ##Formato de texto eje x
        axis.text.y=element_blank() ##Formato de texto eje y
        
  )


## Líneas con fechas en el eje x

head(economics)

# Basica
ggplot(data=economics, aes(x=date, y=pop))+
  geom_line(color="red",linetype = "dashed")

# Con formato
ggplot(data=economics, aes(x=date, y=pop))+
  geom_line(color="red",linetype = "dashed") +
  theme_classic() +                          ## Con esto cambio el tema principal (fondo y colores)
  ggtitle("Línea") +
  xlab("Título eje X") +
  ylab("Título eje Y") +
  theme(plot.title = element_text(hjust = 0.9,color = "black", size =15), ##Formato de título
        axis.title.y=element_text(hjust = 0.5,color = "red", size =13), ##Formato de título eje y
        axis.title.x = element_text(color = "black"),##Formato de título eje x
        axis.text.x=element_text(colour="green", size =11), ##Formato de texto eje x
        axis.text.y=element_blank() ##Formato de texto eje y
        
  )




################################### Gráfica de pie

df <- data.frame(tipo=c("tipo1", "tipo2", "tipo3"),
                 valor=c(4.2, 10, 29.5))
head(df)


## Gràfica básica de tipo pie
ggplot(df, aes(x="", y=valor, fill=tipo)) +
  geom_bar(stat="identity", width=1) + ## hasta aquí es una gráfica de barras
  coord_polar("y", start=0)  ## cambio de coordenadas para hacerla de pie


## doy formato
ggplot(df, aes(x="", y=valor, fill=tipo)) + 
  geom_bar(stat="identity", width=1) + ## hasta aquí es una gráfica de barras
  coord_polar("y", start=0)  +
  theme_bw() +
  theme_classic() +                          ## Con esto cambio el tema principal (fondo y colores)
  ggtitle("Gráfica de pie") +
  xlab("Título eje X") +
  ylab("Título eje Y") +
  theme(
        plot.title = element_text(hjust = 0.2,color = "black", size =15), ##Formato de título
        axis.title.y=element_text(hjust = 0.5,color = "red", size =13), ##Formato de título eje y
        axis.title.x = element_text(color = "black"),##Formato de título eje x
        axis.text.x=element_text(colour="green", size =11), ##Formato de texto eje x
        axis.text.y=element_blank() ##Formato de texto eje y
        
  )




