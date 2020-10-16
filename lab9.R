library(shiny)
library(dplyr)


clean_accidents <- read.csv("./accidentes/datosLimpios.csv")

# renombrar columnas
colnames(clean_accidents)[colnames(clean_accidents) == 'a?.o_ocu'] <- 'anio_ocu'
colnames(clean_accidents)[colnames(clean_accidents) == 'd?.a_ocu'] <- 'dia_ocu'
colnames(clean_accidents)[colnames(clean_accidents) == 'd?.a_sem_ocu'] <- 'dia_sem_ocu'
colnames(clean_accidents)[colnames(clean_accidents) == 'año_ocu'] <- 'anio_ocu'
colnames(clean_accidents)[colnames(clean_accidents) == 'día_ocu'] <- 'dia_ocu'
colnames(clean_accidents)[colnames(clean_accidents) == 'día_sem_ocu'] <- 'dia_sem_ocu'

# Graficas a estudiar
# Color de carro
# bike_colors <- clean_accidents %>%
#   filter(clean_accidents["anio_ocu"] >= 2015 & clean_accidents["anio_ocu"] <= 2018) %>%
#   count(color_veh, sort=TRUE) %>%
#   top_n(10)
# 
# barplot(
#   bike_colors$n, 
#   names.arg=bike_colors$color_veh,
#   main="Color de motocicletas", 
#   xlab="Color", 
#   ylab="Cantidad",
#   col=palette(rainbow(10)),
#   cex.names=.7
# )

# Cantidad de accidentes por anio
yearly_accidents <- clean_accidents %>%
  count(anio_ocu, sort=TRUE)

barplot(
  yearly_accidents$n, 
  names.arg=yearly_accidents$anio_ocu,
  main="Cantidad de accidentes por a?o", 
  xlab="A?o", 
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
    titlePanel("Lab 9: Accidentes en Motocicletas en Guatemala"),
    sidebarLayout(
        position="right",
        sidebarPanel("luis carlos esturban rodriguez"),
        mainPanel(
            textOutput("test_output"),
            sliderInput("year_range", "Coger anio:",
                        min = 2015, max = 2018,
                        value = c(2015, 2018)
            ),
            plotOutput("acc_color", width="50%"),
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
    
    # slider para anio, aca esta como acceder a los valores
    output$acc_color = renderPlot({
      bike_colors <- clean_accidents %>%
        filter(clean_accidents["anio_ocu"] >= input$year_range[1] & clean_accidents["anio_ocu"] <= input$year_range[2]) %>%
        count(color_veh, sort=TRUE) %>%
        top_n(10)
      
      barplot(
        bike_colors$n, 
        names.arg=bike_colors$color_veh,
        main="Color de motocicletas", 
        xlab="Color", 
        ylab="Cantidad",
        col=palette(rainbow(10)),
        cex.names=.7
      )
    })
}

# Create Shiny app ----
shinyApp(ui, server)