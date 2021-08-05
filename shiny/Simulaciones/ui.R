
library(shiny)

shinyUI(fluidPage(
    
    navbarPage(
        "Modelo VAR - Crecimiento económico y sistema financiero en CR",
        id = "tit", 
        
        tabPanel(
            "Escenarios de estrés",
            sidebarLayout(
                sidebarPanel(
                    tags$h3("Generación de datos"),
                    numericInput("n.sim1", "Cantidad de Simulaciones:",
                         min = 1, max = 10000, value = 10), 
                    tags$h4("Seleccione rango de percentiles"),
                    sliderInput("apert_com", "Apertura Comercial", value = c(0.40, 0.55), min = 0, max = 1),
                    sliderInput("cpib.usa", "Crecimiento del PIB USA", value = c(0.01, 0.05), min = 0, max = 1),
                    sliderInput("inf", "Inflación", value = c(0.85, 1), min = 0, max = 1),
                    sliderInput("tbp", "Tasa Básica Pasiva", value = c(0.85, 0.95), min = 0, max = 1),
                    sliderInput("vartc", "Variación anual del Tipo de Cambio", value = c(0.7, 0.95), min = 0, max = 1)),
                mainPanel(
                    tags$h4("Mediana de las estimaciones"),
                    tableOutput("tablas_medianas"),
                    plotOutput("plot1"),
                    plotOutput("plot2"),
                    tags$h4("Datos simulados"),
                    tableOutput("datatable"))
                )
            ), 
        tabPanel(
            "Análisis de Sensibilidad", 
            sidebarLayout(
                sidebarPanel(
                    numericInput("n.sim2", "Cantidad de Simulaciones:",
                                 min = 1, max = 1000, value = 10)), 
                mainPanel(
                    tableOutput("tabla_pibyoy"), 
                    tableOutput("tabla_sf"))
                )
            )
        )
    )
)
    
    
        

