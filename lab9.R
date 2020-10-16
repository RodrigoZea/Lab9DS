library(shiny)
library(dplyr)


clean_accidents <- read.csv("./accidentes/datosLimpios.csv")

# renombrar columnas
colnames(clean_accidents)[colnames(clean_accidents) == 'aÃ.o_ocu'] <- 'anio_ocu'
colnames(clean_accidents)[colnames(clean_accidents) == 'dÃ.a_ocu'] <- 'dia_ocu'
colnames(clean_accidents)[colnames(clean_accidents) == 'dÃ.a_sem_ocu'] <- 'dia_sem_ocu'

# Graficas a estudiar
# Color de carro
bike_colors <- clean_accidents %>%
  filter(clean_accidents["anio_ocu"] >= 2015 & clean_accidents["anio_ocu"] <= 2018) %>%
  count(color_veh, sort=TRUE) %>%
  top_n(10)

barplot(
  bike_colors$n, 
  names.arg=bike_colors$color_veh,
  main="Color de motocicletas", 
  xlab="Color", 
  ylab="Cantidad",
  col=palette(rainbow(10)),
  cex.names=.7,
)

# Cantidad de accidentes por anio
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
    titlePanel("Lab 9: Accidentes en Motocicletas en Guatemala"),
    sidebarLayout(
        position="right",
        sidebarPanel("sidebar panel"),
        mainPanel(
            textOutput("text_output"),
            actionButton(
                inputId="actionButton0",
                label="CLICK!",
                width="200px",
                class="custom-button",
            ),
            textOutput("test_output"),
            plotOutput("plot_data", width="50%"),
        ),
    ),
)

# read data from file
data = read.delim(
    "data/sept_2020.txt", 
    sep="|", 
    header=TRUE,
    fill=TRUE,
    col.names=c(
        "pais",
        "aduana", 
        "fecha", 
        "partida", 
        "modelo",
        "marca", 
        "linea", 
        "cc", 
        "dis", 
        "tipo_veh", 
        "tipo_imp", 
        "comb", 
        "asientos", 
        "puertas", 
        "ton", 
        "cif", 
        "imp",
        "asdf"
    ),
    row.names=NULL
)[, -1]

by_brand = data %>%
    filter(data["tipo_veh"] == "MOTO") %>%
    count(marca, sort=TRUE) %>% 
    top_n(10)

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


    output$plot_data = renderPlot({
        barplot(
            by_brand$n, 
            names.arg=by_brand$marca, 
            main="main title", 
            xlab="x label", 
            ylab="y label",
            col=palette(rainbow(10)),
            cex.names=.7
        )
    })
}

# Create Shiny app ----
shinyApp(ui, server)