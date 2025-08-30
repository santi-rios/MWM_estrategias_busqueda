# Morris Water Maze Analysis App
# Aplicación Shiny para análisis automatizado de experimentos de laberinto acuático de Morris
# Desarrollado por: Santiago Rios

# Cargar librerías necesarias
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(DT)
  library(plotly)
  library(viridis)
  library(dplyr)
  library(ggplot2)
  library(ggprism)
  library(forcats)
  library(readxl)
  library(Rtrack)
})

# Cargar módulos y funciones auxiliares
source("modules/data_input_module.R")
source("modules/arena_config_module.R")
source("modules/analysis_module.R")
source("modules/results_module.R")
source("utils/processing_functions.R")
source("utils/plotting_functions.R")

# UI
ui <- dashboardPage(
  dashboardHeader(
    title = "MWM Analysis Tool",
    titleWidth = 250
  ),
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("📁 Cargar Datos", tabName = "data_input", icon = icon("upload")),
      menuItem("🎯 Configurar Arena", tabName = "arena_config", icon = icon("bullseye")),
      menuItem("⚙️ Procesar Análisis", tabName = "analysis", icon = icon("cogs")),
      menuItem("📊 Resultados", tabName = "results", icon = icon("chart-line")),
      menuItem("💾 Exportar", tabName = "export", icon = icon("download")),
      br(),
      div(
        style = "padding: 20px; text-align: center;",
        h5("Morris Water Maze", style = "color: #3c8dbc;"),
        p("Análisis automatizado de estrategias de búsqueda", 
          style = "font-size: 12px; color: #666;")
      )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$script(HTML("
        $(document).ready(function() {
          $('.content-wrapper, .right-side').css('background-color', '#f4f4f4');
        });
      "))
    ),
    
    tabItems(
      # Tab 1: Cargar Datos
      tabItem(
        tabName = "data_input",
        fluidRow(
          box(
            title = "🗂️ Configuración de Archivos de Datos",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            dataInputUI("data_input")
          )
        )
      ),
      
      # Tab 2: Configurar Arena
      tabItem(
        tabName = "arena_config",
        fluidRow(
          arenaConfigUI("arena_config")
        )
      ),
      
      # Tab 3: Análisis
      tabItem(
        tabName = "analysis",
        fluidRow(
          analysisUI("analysis")
        )
      ),
      
      # Tab 4: Resultados
      tabItem(
        tabName = "results",
        fluidRow(
          resultsUI("results")
        )
      ),
      
      # Tab 5: Exportar
      tabItem(
        tabName = "export",
        fluidRow(
          box(
            title = "💾 Exportar Resultados",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            
            h4("Descargar Análisis Completo"),
            p("Descarga todos los resultados generados en formato ZIP."),
            br(),
            
            div(
              style = "text-align: center;",
              downloadButton("downloadResults", 
                           "📦 Descargar Todo",
                           class = "btn-success btn-lg",
                           style = "margin: 20px;")
            ),
            
            hr(),
            
            h4("Descargas Individuales"),
            fluidRow(
              column(4,
                     downloadButton("downloadDensityMaps", 
                                  "🗺️ Mapas de Densidad",
                                  class = "btn-info")
              ),
              column(4,
                     downloadButton("downloadStrategies", 
                                  "🎯 Análisis de Estrategias",
                                  class = "btn-warning")
              ),
              column(4,
                     downloadButton("downloadData", 
                                  "📊 Datos Procesados",
                                  class = "btn-secondary")
              )
            )
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Valores reactivos para compartir datos entre módulos
  values <- reactiveValues(
    experiment_data = NULL,
    arena_config = NULL,
    processed_data = NULL,
    strategies = NULL,
    density_plots = NULL,
    strategy_plots = NULL,
    processing_complete = FALSE
  )
  
  # Llamar a los módulos del servidor
  dataInputServer("data_input", values)
  arenaConfigServer("arena_config", values)
  analysisServer("analysis", values)
  resultsServer("results", values)
  
  # Descargas
  output$downloadResults <- downloadHandler(
    filename = function() {
      paste("MWM_Analysis_", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      # Crear un directorio temporal
      temp_dir <- tempdir()
      
      # Guardar todos los archivos
      if (!is.null(values$density_plots)) {
        ggsave(file.path(temp_dir, "density_maps.png"), 
               values$density_plots, width = 12, height = 8, dpi = 300)
      }
      
      if (!is.null(values$strategy_plots)) {
        ggsave(file.path(temp_dir, "strategy_analysis.png"), 
               values$strategy_plots, width = 10, height = 6, dpi = 300)
      }
      
      if (!is.null(values$processed_data)) {
        write.csv(values$processed_data, file.path(temp_dir, "processed_data.csv"), row.names = FALSE)
      }
      
      # Crear el ZIP
      zip(file, list.files(temp_dir, full.names = TRUE))
    }
  )
  
  # Mensaje de bienvenida
  observeEvent(TRUE, {
    showModal(modalDialog(
      title = "🎯 Bienvenido al Analizador de MWM",
      HTML("
        <h4>¡Hola! Esta aplicación te ayudará a analizar tus experimentos del Laberinto Acuático de Morris.</h4>
        <br>
        <h5>📋 Pasos a seguir:</h5>
        <ol>
          <li><strong>Cargar Datos:</strong> Sube tu archivo de experimento (.xlsx) y archivos de tracks</li>
          <li><strong>Configurar Arena:</strong> Define las dimensiones y objetivos de tu arena</li>
          <li><strong>Procesar:</strong> Ejecuta el análisis automático</li>
          <li><strong>Resultados:</strong> Visualiza mapas de densidad y análisis de estrategias</li>
          <li><strong>Exportar:</strong> Descarga todos los resultados</li>
        </ol>
        <br>
        <p><em>¡Comencemos cargando tus datos!</em></p>
      "),
      easyClose = TRUE,
      footer = modalButton("¡Empezar!")
    ))
  }, once = TRUE)
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
