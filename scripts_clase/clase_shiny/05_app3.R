#initialize
library(shiny)
library(ggplot2)
library(purrr)
library(dplyr)
library(shinyWidgets)


#datos
data <- mtcars %>% 
  count(cyl) %>%
  mutate(cyl = factor(cyl)) %>% 
  rename(Country = cyl, Count = n)


# UI for app
ui<-(
  pageWithSidebar(
    # title
    headerPanel("Selecciona"),
    
    #input
    sidebarPanel(
      
      radioGroupButtons(
        inputId = "change_plot",
        label = "Tipo de gráfica:",
        choices = c(
          "Barras" = "bar",
          "Pie" = "pie"
        ),
        justified = TRUE,
        selected = "bar"
      ),
      
      textInput(
        inputId = "input_titulo",
        label = "Ingresa un título:"
      )
      
      
    ),
    
    # output
    mainPanel(
      plotOutput("plot1", height = 250)
    )
  ))


# shiny server side code for each call
server<-(function(input, output, session){
  
  output$plot1 <- renderPlot({
    if (input$change_plot %in% "bar") {
      
      ggplot(data, aes(Country, Count, fill = Country)) +
        geom_bar(stat = "identity")+
        ggtitle(input$input_titulo) +
        xlab("Título eje X") +
        ylab("Título eje Y") +
        theme(plot.title = element_text(hjust = 0.9,color = "black", size =15), ##Formato de título
              axis.title.y=element_text(hjust = 0.5,color = "red", size =13), ##Formato de título eje y
              axis.title.x = element_text(color = "black"),##Formato de título eje x
              axis.text.x=element_text(colour="green", size =11), ##Formato de texto eje x
              axis.text.y=element_blank() ##Formato de texto eje y
              
        )
      
    } else {
      
      ggplot(data, aes(x = "", y = Count, fill = Country)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        ggtitle(input$input_titulo) +
        xlab("Título eje X") +
        ylab("Título eje Y") +
        theme(plot.title = element_text(hjust = 0.9,color = "black", size =15), ##Formato de título
              axis.title.y=element_text(hjust = 0.5,color = "red", size =13), ##Formato de título eje y
              axis.title.x = element_text(color = "black"),##Formato de título eje x
              axis.text.x=element_text(colour="green", size =11), ##Formato de texto eje x
              axis.text.y=element_blank() ##Formato de texto eje y
              
        )
    }
  })
  
})


# Create Shiny app ----
shinyApp(ui, server)