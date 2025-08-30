# Módulo para visualización de resultados
# Muestra mapas de densidad y análisis de estrategias

resultsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
        conditionalPanel(
          condition = paste0("!output['", ns("results_available"), "']"),
          box(
            title = "📊 Resultados",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            
            div(
              style = "text-align: center; padding: 40px;",
              icon("exclamation-triangle", style = "font-size: 48px; color: #f39c12;"),
              h4("No hay resultados disponibles"),
              p("Completa el procesamiento en la pestaña 'Procesar Análisis' para ver los resultados.")
            )
          )
        )
      )
    ),
    
    conditionalPanel(
      condition = paste0("output['", ns("results_available"), "']"),
      
      # Tabs de resultados
      tabsetPanel(
        id = ns("results_tabs"),
        
        # Tab: Mapas de Densidad
        tabPanel("🗺️ Mapas de Densidad",
          br(),
          fluidRow(
            box(
              title = "Configuración de Mapas",
              status = "primary",
              solidHeader = TRUE,
              width = 4,
              
              h5("Filtros de Datos"),
              
              selectInput(ns("density_grouping"),
                         "Agrupar por:",
                         choices = NULL),
              
              selectInput(ns("density_days"),
                         "Días a incluir:",
                         choices = NULL,
                         multiple = TRUE),
              
              hr(),
              
              h5("Configuración Visual"),
              
              selectInput(ns("color_palette"),
                         "Paleta de colores:",
                         choices = c("Viridis" = "viridis",
                                   "Plasma" = "plasma",
                                   "Inferno" = "inferno",
                                   "Magma" = "magma",
                                   "Cividis" = "cividis"),
                         selected = "viridis"),
              
              numericInput(ns("resolution"),
                          "Resolución:",
                          value = 900,
                          min = 100,
                          max = 2000,
                          step = 100),
              
              numericInput(ns("color_levels"),
                          "Niveles de color:",
                          value = 300,
                          min = 50,
                          max = 500,
                          step = 50),
              
              checkboxInput(ns("show_legend"),
                           "Mostrar leyenda",
                           value = FALSE),
              
              br(),
              
              actionButton(ns("generate_density_maps"),
                          "🗺️ Generar Mapas",
                          class = "btn-primary")
            ),
            
            box(
              title = "Mapas de Densidad",
              status = "success",
              solidHeader = TRUE,
              width = 8,
              
              conditionalPanel(
                condition = paste0("!output['", ns("density_maps_ready"), "']"),
                div(
                  style = "text-align: center; padding: 40px; color: #666;",
                  icon("map", style = "font-size: 48px;"),
                  h4("Configurar y generar mapas"),
                  p("Selecciona los parámetros y presiona 'Generar Mapas'")
                )
              ),
              
              conditionalPanel(
                condition = paste0("output['", ns("density_maps_ready"), "']"),
                plotOutput(ns("density_plots"), height = "600px")
              )
            )
          )
        ),
        
        # Tab: Análisis de Estrategias
        tabPanel("🎯 Estrategias de Búsqueda",
          br(),
          fluidRow(
            box(
              title = "Configuración de Análisis",
              status = "primary",
              solidHeader = TRUE,
              width = 4,
              
              h5("Clasificación de Estrategias"),
              
              div(
                style = "background: #f8f9fa; padding: 10px; border-radius: 5px; margin: 10px 0;",
                h6("🧠 Estrategias Alocéntricas (Hipocampo-dependientes):"),
                tags$ul(
                  tags$li("Directed search"),
                  tags$li("Corrected path"),
                  tags$li("Direct path")
                ),
                
                h6("🔄 Estrategias Egocéntricas (Hipocampo-independientes):"),
                tags$ul(
                  tags$li("Thigmotaxis"),
                  tags$li("Circling"),
                  tags$li("Random path"),
                  tags$li("Scanning"),
                  tags$li("Chaining")
                ),
                
                h6("⚡ Otras:"),
                tags$ul(
                  tags$li("Perseverance")
                )
              ),
              
              hr(),
              
              h5("Configuración de Análisis"),
              
              selectInput(ns("strategy_grouping"),
                         "Agrupar por:",
                         choices = NULL),
              
              selectInput(ns("strategy_days"),
                         "Días a incluir:",
                         choices = NULL,
                         multiple = TRUE),
              
              checkboxInput(ns("show_individual_strategies"),
                           "Mostrar estrategias individuales",
                           value = FALSE),
              
              br(),
              
              actionButton(ns("generate_strategy_analysis"),
                          "🎯 Generar Análisis",
                          class = "btn-primary")
            ),
            
            box(
              title = "Análisis de Estrategias",
              status = "success",
              solidHeader = TRUE,
              width = 8,
              
              conditionalPanel(
                condition = paste0("!output['", ns("strategy_plots_ready"), "']"),
                div(
                  style = "text-align: center; padding: 40px; color: #666;",
                  icon("bullseye", style = "font-size: 48px;"),
                  h4("Configurar y generar análisis"),
                  p("Selecciona los parámetros y presiona 'Generar Análisis'")
                )
              ),
              
              conditionalPanel(
                condition = paste0("output['", ns("strategy_plots_ready"), "']"),
                plotOutput(ns("strategy_plots"), height = "500px")
              )
            )
          ),
          
          fluidRow(
            box(
              title = "📈 Análisis Estadístico",
              status = "info",
              solidHeader = TRUE,
              width = 12,
              
              conditionalPanel(
                condition = paste0("output['", ns("stats_available"), "']"),
                
                h5("Modelo de Regresión"),
                verbatimTextOutput(ns("statistical_summary")),
                
                hr(),
                
                h5("Tabla de Estrategias por Grupo"),
                DT::dataTableOutput(ns("strategy_table"))
              )
            )
          )
        ),
        
        # Tab: Datos Procesados
        tabPanel("📊 Datos Procesados",
          br(),
          fluidRow(
            box(
              title = "Resumen de Tracks",
              status = "info",
              solidHeader = TRUE,
              width = 6,
              
              DT::dataTableOutput(ns("tracks_summary"))
            ),
            
            box(
              title = "Métricas por Track",
              status = "info",
              solidHeader = TRUE,
              width = 6,
              
              DT::dataTableOutput(ns("metrics_summary"))
            )
          ),
          
          fluidRow(
            box(
              title = "Estrategias Llamadas",
              status = "success",
              solidHeader = TRUE,
              width = 12,
              
              DT::dataTableOutput(ns("strategies_table"))
            )
          )
        )
      )
    )
  )
}

resultsServer <- function(id, values, parent_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Variables reactivas para plots
    density_maps <- reactiveVal(NULL)
    strategy_plots <- reactiveVal(NULL)
    statistical_results <- reactiveVal(NULL)
    
    # Verificar si hay resultados disponibles
    output$results_available <- reactive({
      !is.null(values$processed_data) && !is.null(values$strategies)
    })
    outputOptions(output, "results_available", suspendWhenHidden = FALSE)
    
    # Actualizar opciones cuando hay datos disponibles
    observe({
      req(values$processed_data, values$strategies)
      
      # Obtener factores únicos
      factors_data <- values$processed_data$factors
      factor_names <- names(factors_data)[!grepl("^_", names(factors_data))]
      day_names <- names(factors_data)[grepl("^_Day", names(factors_data))]
      
      # Actualizar selects de mapas de densidad
      updateSelectInput(session, "density_grouping",
                       choices = factor_names)
      
      if (length(day_names) > 0) {
        day_values <- sort(unique(factors_data[[day_names[1]]]))
        updateSelectInput(session, "density_days",
                         choices = day_values,
                         selected = day_values)
      }
      
      # Actualizar selects de estrategias
      updateSelectInput(session, "strategy_grouping",
                       choices = factor_names)
      
      updateSelectInput(session, "strategy_days",
                       choices = day_values,
                       selected = day_values)
    })
    
    # Generar mapas de densidad
    observeEvent(input$generate_density_maps, {
      req(values$processed_data, input$density_grouping, input$density_days)
      
      tryCatch({
        showNotification("Generando mapas de densidad...", type = "default")
        
        # Crear mapas de densidad agrupados
        plots <- create_density_maps(
          experiment_data = values$processed_data,
          grouping_var = input$density_grouping,
          days_filter = input$density_days,
          color_palette = input$color_palette,
          resolution = input$resolution,
          color_levels = input$color_levels,
          show_legend = input$show_legend
        )
        
        density_maps(plots)
        values$density_plots <- plots
        
        showNotification("Mapas de densidad generados correctamente", type = "success")
        
      }, error = function(e) {
        showNotification(paste("Error generando mapas:", e$message), type = "error")
      })
    })
    
    # Generar análisis de estrategias
    observeEvent(input$generate_strategy_analysis, {
      req(values$strategies, input$strategy_grouping, input$strategy_days)
      
      tryCatch({
        showNotification("Generando análisis de estrategias...", type = "default")
        
        # Crear análisis de estrategias
        results <- create_strategy_analysis(
          strategies_data = values$strategies,
          experiment_data = values$processed_data,
          grouping_var = input$strategy_grouping,
          days_filter = input$strategy_days,
          show_individual = input$show_individual_strategies
        )
        
        strategy_plots(results$plots)
        statistical_results(results$stats)
        values$strategy_plots <- results$plots
        values$statistical_results <- results$stats
        
        showNotification("Análisis de estrategias generado correctamente", type = "success")
        
      }, error = function(e) {
        showNotification(paste("Error generando análisis:", e$message), type = "error")
      })
    })
    
    # Outputs de estado
    output$density_maps_ready <- reactive({
      !is.null(density_maps())
    })
    outputOptions(output, "density_maps_ready", suspendWhenHidden = FALSE)
    
    output$strategy_plots_ready <- reactive({
      !is.null(strategy_plots())
    })
    outputOptions(output, "strategy_plots_ready", suspendWhenHidden = FALSE)
    
    output$stats_available <- reactive({
      !is.null(statistical_results())
    })
    outputOptions(output, "stats_available", suspendWhenHidden = FALSE)
    
    # Renderizar plots
    output$density_plots <- renderPlot({
      req(density_maps())
      density_maps()
    })
    
    output$strategy_plots <- renderPlot({
      req(strategy_plots())
      strategy_plots()
    })
    
    # Tablas de datos
    output$tracks_summary <- DT::renderDataTable({
      req(values$processed_data)
      
      summary_data <- data.frame(
        Track_ID = values$processed_data$factors$`_TrackID`,
        Subject_ID = values$processed_data$factors$`_TargetID`,
        Day = values$processed_data$factors$`_Day`,
        Trial = values$processed_data$factors$`_Trial`
      )
      
      DT::datatable(
        summary_data,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = 'ltip'
        ),
        class = 'cell-border stripe hover'
      )
    })
    
    output$strategies_table <- DT::renderDataTable({
      req(values$strategies)
      
      strategies_df <- values$strategies$calls
      strategies_df$Track_ID <- rownames(strategies_df)
      
      DT::datatable(
        strategies_df,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = 'ltip'
        ),
        class = 'cell-border stripe hover'
      ) %>%
        DT::formatRound(columns = c("confidence", as.character(1:9)), digits = 3)
    })
    
    output$statistical_summary <- renderPrint({
      req(statistical_results())
      if (!is.null(statistical_results()$model)) {
        summary(statistical_results()$model)
      } else {
        "Análisis estadístico no disponible"
      }
    })
  })
}
