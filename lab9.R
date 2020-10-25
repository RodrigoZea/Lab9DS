library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

clean_accidents <- read.csv("./accidentes/datosLimpios.csv")

# renombrar columnas
colnames(clean_accidents)[colnames(clean_accidents) == 'aÃ.o_ocu'] <- 'anio_ocu'
colnames(clean_accidents)[colnames(clean_accidents) == 'dÃ.a_ocu'] <- 'dia_ocu'
colnames(clean_accidents)[colnames(clean_accidents) == 'dÃ.a_sem_ocu'] <- 'dia_sem_ocu'
#colnames(clean_accidents)[colnames(clean_accidents) == 'aÃ±o_ocu'] <- 'anio_ocu'
#colnames(clean_accidents)[colnames(clean_accidents) == 'dÃ?a_ocu'] <- 'dia_ocu'
#colnames(clean_accidents)[colnames(clean_accidents) == 'dÃ?a_sem_ocu'] <- 'dia_sem_ocu'

# Graficas a estudiar
# Cantidad de accidentes por anio


pie <- data.frame(table(clean_accidents$sexo_per))
pie = pie[-1,]

yearly_accidents <- clean_accidents %>%
  count(anio_ocu, sort=TRUE)

barplot(
  yearly_accidents$n, 
  names.arg=yearly_accidents$anio_ocu,
  main="Cantidad de accidentes por año", 
  xlab="Año", 
  ylab="Accidentes",
  col=palette(rainbow(10)),
  cex.names=.7,
)

# Ocurrencias de accidentes por mes
monthly_accidents <- clean_accidents %>%
  filter(clean_accidents["anio_ocu"] == 2015) %>%
  count(mes_ocu, sort=TRUE)

barplot(
  monthly_accidents$n, 
  names.arg=monthly_accidents$mes_ocu,
  main="Cantidad de accidentes mensualmente", 
  xlab="Meses", 
  ylab="Accidentes",
  col=palette(rainbow(10)),
  cex.names=.7,
)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
    includeCSS("styles.css"),
    headerPanel(
      div(
        h1("LAB 9: ACCIDENTES EN MOTOCICLETAS EN GUATEMALA", id = "title1"),
        id = "headPanel"
      )
    ),
    
    sidebarLayout(
        position="right",
        sidebarPanel(
          tags$style(".well {background-color:#003049;}"),
          h3("INTEGRANTES DEL EQUIPO:"),
          h4("Sebastian Arriola"),
          h4("Rodrigo Zea"),
          h4("Gustavo de Leon"),
        ),
        mainPanel(
            # Plot de accidentes agrupados por color y slider para elegir rango
            # de aÃ±os.
            fluidRow(
              align="center",
              plotOutput("acc_color", width="100%"),
            ),
            fluidRow(
              align="center",
              sliderInput("slider0", "Rango de Años:",
                          min = 2015, max = 2018,
                          value = c(2015, 2018)
              ),
            ),
            
            # Plot de accidentes por aÃ±o
            fluidRow(
              align="center",
              plotOutput("acc_yearly", width="100%"),
            ),
            
            fluidRow(
              align="center",
              radioButtons(
                "radio_button0",
                "Ordenar",
                c("Por cantidad", "Por año")
              ),
            ),
            
            # Plot de accidentes por aÃ±o
            fluidRow(
              align="center",
              plotlyOutput("pie", width="100%"),
            ),
            
        ),
    ),
)

server = function(input, output, session) {
    # state
    count = reactiveValues(value=0)

    # button callbacks
    observeEvent(input$actionButton0, {
        count$value = count$value + 1
    })
    
    output$text_output = renderText({
        paste(
            "clicked ",
            count$value,
            " times!"
        )
    })
    
    # Grafica de accidentes agrupados por color, se accede a los valores de los
    # sliders para definir los aÃ±os para filtrar los datos.
    output$acc_color = renderPlot({
      bike_colors <- clean_accidents %>%
        filter(clean_accidents["anio_ocu"] >= input$slider0[1] & clean_accidents["anio_ocu"] <= input$slider0[2]) %>%
        count(color_veh, sort=TRUE) %>%
        top_n(10)
      
      par(bg = '#eae2b7')
      barplot(
        bike_colors$n, 
        names.arg=bike_colors$color_veh,
        main="Accidentes por Color y Años", 
        border=NA,
        xlab="Color", 
        ylab="Cantidad",
        col="#f77f00",
        cex.names=.7
      )
    })
    
    # Grafica de accidentes por aÃ±o
    output$acc_yearly = renderPlot({
      if (input$radio_button0 == "Por cantidad") {
        yearly_accidents <- clean_accidents %>%
          count(anio_ocu, sort=TRUE)
      } else {
        yearly_accidents <- clean_accidents %>%          
          count(anio_ocu, sort=FALSE)
      }
      
      par(bg = '#eae2b7')
      barplot(
        yearly_accidents$n, 
        names.arg=yearly_accidents$anio_ocu,
        main="Cantidad de accidentes por año", 
        border=NA,
        xlab="Anio", 
        ylab="Accidentes",
        col= "#d62828",
        cex.names=.7,
      )
    })
    
    output$pie = renderPlotly({

      data <- data.frame(
        Sexo=c("Mujer","Hombre"),
        value=c(35,611)
      )
      
      fig <- plot_ly(data, labels = ~Sexo, values = ~value, type = 'pie')
      fig <- fig %>% layout(title = 'Cantidad de accidentes con hombres y mujeres involucrados',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            plot_bgcolor='rgb(254, 247, 234)',
                            paper_bgcolor='transparent'
                            )
      
      
      
    })
}

# Create Shiny app ----
shinyApp(ui, server)



