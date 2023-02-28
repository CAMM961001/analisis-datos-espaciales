## Script de estructura general de una app de shiny parte 2

library(shiny)
library(tidyverse)
library(ggplot2)

# UI: Se define el front end: Títulos, en dónde hay gráficas, etc.
# Server: hace las operaciones
# shinyApp(ui = ui, server = server) esto siempre va hasta el final

# Además hay otros elementos como
# inputs: Son los parámetros que permiten seleccionar cosas o ingresar información
# Regularmente estos inputs sirven para ser procesados en el server y generan un output
# output: los outputs son los resultados de lo que se procesa en el server, hay distintos tipos: Gráficas, numerico, texto, tablas.

## Puedo cargar datos o información que voy a estar utilizando desde antes de correr el UI y server
datos <- mtcars
datos <-datos %>% 
  mutate(`car name`=rownames(datos))

ui <- fluidPage(
  
  # Título
  titlePanel("Hola shiny 2"),
  
  # Defino un layout general llamado sidebarLayout que tiene
  # Un panel de seleccion al costado y un panel grande en el que se visualiza el resultado
  
  sidebarLayout(
    # sidebarPanel: Aquí se definen los elementos de la barra lateral
    sidebarPanel(
      
      # Input:
      textInput(inputId = "input_texto",      # Nombre del input, este lo vamos a usar para ingresar la info
                label = "Ingresa texto:",  #  Encabezado
                value = ""),                # Valor inicial
      
      h4(" Este número se va a elevar al cuadrado"), # Dentro de h4() puedo colocar texto
      
      numericInput(inputId = "input_numero",      # Nombre del input, este lo vamos a usar para ingresar la info
                   label = "Ingresa numero del 1 al 10:",  #  Encabezado
                   value = "")  ,              # Valor inicial,
      
      h4(" Selecciona un estado"), # Dentro de h4() puedo colocar texto 
      
      selectInput("estado", "Choose a state:",
                  list(`East Coast` = list("NY", "NJ", "CT"),
                       `West Coast` = list("WA", "OR", "CA"),
                       `Midwest` = list("MN", "WI", "IA")
                       )
      ),
      
      h4("Número de filas de un dataframe que voy a ver"), # Dentro de h4() puedo colocar texto
      numericInput(inputId = "input_filas",      # Nombre del input, este lo vamos a usar para ingresar la info
                   label = "Ingresa numero de filas que quieres ver:",  #  Encabezado
                   value = 2) ,              # Valor inicial
      
      h4("Para hacer una gráfica"), # Dentro de h4() puedo colocar texto
      
      ## Voy a seleccionar tipo de carro y la variable númerica
      selectInput(inputId = "input_carro",      
                   label = "Ingresa el tipo de carro :",  
                   unique(datos$`car name`),
                  multiple = TRUE ## En este caso voy a seleccionar diferentes tipos de carro
                  ),     
      
      selectInput(inputId = "input_variable",      
                  label = "Ingresa variable a medir :",  
                 mtcars %>% 
                   select(mpg,disp,hp) %>% 
                   names(), ## Aquí seleccioné tres variables numéricas y enseño los nombres
                  multiple = TRUE ## En este caso voy a seleccionar diferentes tipos de carro
      ), 
      
    ),
    
    # Main panel: Aquí se mdefinen los elementos del panel principal
    mainPanel(
      
      h2("Texto que ingresaste:"),
      
      textOutput(outputId = "output_texto"), 
      
      h2("Número al cuadrado:"),
      textOutput(outputId = "output_cuadrado"), 
      
      h2("Estado que seleccionaste:"),
      textOutput(outputId = "output_estado"), 
      
      h2("Tabla:"),
      tableOutput(outputId = "output_tabla"), 
      
      h2("Gráfica:"),
      plotOutput(outputId = "output_plot")
      
    )
    
   
    
  ),
  
  

  
)
    


server <- function(input, output) {
  
  ## Se define un output  
  
  output$output_texto <- renderText({  ## Defino el tipo de output, nombreo el output con output$nombre
    
    x    <- input$input_texto   #uso input$ para llamar los diferentes inputs, 
    x
    
  })
  
  output$output_cuadrado <- renderText({  ## Defino el tipo de output, nombreo el output con output$nombre
    
    ## Aquí hago la operación con el input_número
    x    <- (input$input_numero )^2  
    x
    
  })

  
  output$output_estado <- renderText({
    paste("Seleccionaste el estado", input$estado)
  })
  
  output$output_tabla <- renderTable({
  # Defino el dataframe
   tabla <- mtcars
   
   
   print(input$input_filas)
   print(class(input$filas))
   
   # filto en número de filas que seleccioné
   tabla <- tabla %>% 
     head(as.numeric(input$input_filas))
   tabla
   
  })
  
  output$output_plot <- renderPlot({
    
    ## Utilizo los inputs para modificar el dataframe, sobre todo filtros
    
    data1 <- datos %>% 
      filter(`car name` %in% input$input_carro) %>% 
      select(`car name`,input$input_variable) 
    
    # Nombro la segunda columna como "valor" para que se ajuste al script de la gráfica
    colnames(data1)[2]<-"valor"
    
    print("aqui")
    print(data1)
    
    ggplot(data = data1, aes(x = `car name`, y= valor, fill= `car name`)) + 
      geom_bar(stat = "identity") +
      ggtitle("Gráfica de barras") +
      xlab("Carro") +
      ylab(input$input_variable)
  })
  
  
}

shinyApp(ui = ui, server = server)