library(shiny)
library(dplyr)

# Define UI for app that draws a histogram ----
ui <- fluidPage(

    includeCSS("styles.css"),
    titlePanel("Shiny App"),
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