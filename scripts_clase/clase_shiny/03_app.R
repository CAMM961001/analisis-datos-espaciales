## Script de estructura general de una app de shiny

library(shiny)

# UI: Se define el front end: Títulos, en dónde hay gráficas, etc.
# Server: hace las operaciones
# shinyApp(ui = ui, server = server) esto siempre va hasta el final y compila la aplicación

# Además hay otros elementos como
# inputs: Son los parámetros que permiten seleccionar cosas o ingresar información
# Regularmente estos inputs sirven para ser procesados en el server y generan un output
# output: los outputs son los resultados de lo que se procesa en el server, hay distintos tipos: Gráficas, numerico, texto, tablas.


ui <- fluidPage(
  
  # Título
  titlePanel("Hello Shiny!"),
  
  # Defino un layout general llamado sidebarLayout que tiene
  # Un panel de seleccion al costado y un panel grande en el que se visualiza el resultado
  
  sidebarLayout(
    # sidebarPanel: Aquí se definen los elementos de la barra lateral
    sidebarPanel(
      
      # Input:
      textInput(inputId = "input_texto",      # Nombre del input, este lo vamos a usar para ingresar la info
                  label = "Ingresa texto:",  #  Encabezado
                  value = "")                # Valor inicial
      
    ),
    
    # Main panel: Aquí se mdefinen los elementos del panel principal
    mainPanel(
      
      # Output: Histogram ----
      textOutput(outputId = "output_texto") # Aquí defino qué output voy a mostrar
      
    )
  )
)

server <- function(input, output) {
  
  ## Se define un output  o
  
  output$output_texto <- renderText({  ## Defino el tipo de output, nombreo el output con output$nombre
    
    x    <- input$input_texto   #uso input$ para llamar los diferentes inputs, 
    x <- paste0('Ingresaste:', x)
    x
    
  })
  
}

shinyApp(ui = ui, server = server)
