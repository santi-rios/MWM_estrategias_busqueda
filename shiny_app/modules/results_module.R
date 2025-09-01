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
    # En resultsUI
    fluidRow(
      column(4, 
        selectizeInput(ns("probe_filter"), "Filtrar por Probe:", 
                      choices = c("Todos", "Solo entrenamiento (Probe = FALSE)", 
                                 "Solo pruebas (Probe = TRUE)"),
                      selected = "Todos")
      ),
      column(4,
        selectizeInput(ns("arena_filter"), "Filtrar por Arena:", 
                      choices = NULL, # Se actualiza dinámicamente
                      selected = NULL,
                      multiple = TRUE)
      )
    ),
    # En resultsUI, añadir info sobre Probe y Arena
    tags$div(
      class = "alert alert-info",
      tags$h4("🔍 Filtros importantes:"),
      tags$p(HTML("<strong>Probe:</strong> Permite separar fases de entrenamiento (Probe=FALSE) de las fases de prueba (Probe=TRUE).")),
      tags$p(HTML("<strong>Arena:</strong> Permite filtrar por tipo de arena definida en tu archivo de experimento. 
                 Útil para separar configuraciones diferentes (ej: plataforma en posición normal vs. reversa).")),
      tags$p("Estos filtros te ayudarán a analizar específicamente cada fase experimental.")
    ),
    conditionalPanel(
      condition = paste0("output['", ns("results_available"), "']"),
      
      # Tabs de resultados
      tabsetPanel(
        id = ns("results_tabs"),
        
        # Tab: Mapas de Densidad
        tabPanel("🗺️ Mapas de Densidad",
          br(),
          uiOutput(ns("density_filter_status")),
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
              
              selectInput(ns("density_arenas"),
                         "Arenas a incluir:",
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
          uiOutput(ns("strategy_filter_status")),
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
          uiOutput(ns("data_filter_status")),
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
        ),
        
        # Tab: Resultados Exportados
        tabPanel("📈 Análisis Completo",
          br(),
          uiOutput(ns("analysis_filter_status")),
          fluidRow(
            box(
              title = "Configuración de Análisis",
              status = "primary",
              solidHeader = TRUE,
              width = 4,
              
              h5("Variable a Analizar"),
              
              selectInput(ns("analysis_variable"),
                         "Variable de interés:",
                         choices = NULL),
              
              selectInput(ns("analysis_grouping"),
                         "Agrupar por:",
                         choices = NULL),
              
              selectInput(ns("analysis_days"),
                         "Días a incluir:",
                         choices = NULL,
                         multiple = TRUE),
              
              checkboxInput(ns("show_error_bars"),
                           "Mostrar barras de error (±SE)",
                           value = TRUE),
              
              checkboxInput(ns("log_transform"),
                           "Transformación logarítmica",
                           value = FALSE),
              
              br(),
              
              actionButton(ns("generate_analysis"),
                          "📊 Generar Análisis",
                          class = "btn-primary"),
              
              br(), br(),
              
              h5("Análisis Estadístico"),
              
              selectInput(ns("stat_test"),
                         "Prueba estadística:",
                         choices = c(
                           "ANOVA" = "anova",
                           "t-test" = "ttest",
                           "Kruskal-Wallis" = "kruskal",
                           "Mann-Whitney" = "wilcox",
                           "Poisson GLM (log, LRT interacción)" = "poisson"
                         )
              ),
              
              actionButton(ns("run_statistics"),
                          "📈 Ejecutar Estadística",
                          class = "btn-info")
            ),
            
            box(
              title = "Gráfico de Análisis",
              status = "success",
              solidHeader = TRUE,
              width = 8,
              
              conditionalPanel(
                condition = paste0("!output['", ns("analysis_plots_ready"), "']"),
                div(
                  style = "text-align: center; padding: 40px; color: #666;",
                  icon("chart-bar", style = "font-size: 48px;"),
                  h4("Configurar y generar análisis"),
                  p("Selecciona los parámetros y presiona 'Generar Análisis'")
                )
              ),
              
              conditionalPanel(
                condition = paste0("output['", ns("analysis_plots_ready"), "']"),
                plotOutput(ns("analysis_plots"), height = "500px")
              )
            )
          ),
          
          fluidRow(
            box(
              title = "Resultados Estadísticos",
              status = "info",
              solidHeader = TRUE,
              width = 6,
              
              conditionalPanel(
                condition = paste0("output['", ns("stats_results_available"), "']"),
                verbatimTextOutput(ns("statistical_results"))
              )
            ),
            
            box(
              title = "Datos Exportados Completos",
              status = "success",
              solidHeader = TRUE,
              width = 6,
              
              DT::dataTableOutput(ns("exported_results_table"))
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
    analysis_plots <- reactiveVal(NULL)
    stats_results <- reactiveVal(NULL)
    
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
      arena_names <- names(factors_data)[grepl("^_Arena", names(factors_data))]
      
      # Actualizar selects de mapas de densidad
      updateSelectInput(session, "density_grouping",
                       choices = factor_names)
      
      if (length(day_names) > 0) {
        day_values <- sort(unique(factors_data[[day_names[1]]]))
        updateSelectInput(session, "density_days",
                         choices = day_values,
                         selected = day_values)
      } else {
        updateSelectInput(session, "density_days", choices = NULL)
      }
      
      if (length(arena_names) > 0) {
        arena_values <- unique(factors_data[[arena_names[1]]])
        updateSelectInput(session, "density_arenas",
                         choices = arena_values,
                         selected = arena_values)
      } else {
        updateSelectInput(session, "density_arenas", choices = NULL)
      }
      
      # Actualizar selects de estrategias
      updateSelectInput(session, "strategy_grouping",
                       choices = factor_names)
      
      if (length(day_names) > 0) {
        day_values <- sort(unique(factors_data[[day_names[1]]]))
        updateSelectInput(session, "strategy_days",
                         choices = day_values,
                         selected = day_values)
      } else {
        updateSelectInput(session, "strategy_days", choices = NULL)
      }
      
      # Actualizar selects para análisis completo
      if (!is.null(values$analysis_config) && !is.null(values$analysis_config$results_export)) {
        numeric_vars <- names(values$analysis_config$results_export)[
          sapply(values$analysis_config$results_export, is.numeric)
        ]
        
        updateSelectInput(session, "analysis_variable",
                         choices = numeric_vars,
                         selected = if("velocity" %in% numeric_vars) "velocity" else numeric_vars[1])
        
        updateSelectInput(session, "analysis_grouping",
                         choices = factor_names)
        
        updateSelectInput(session, "analysis_days",
                         choices = day_values,
                         selected = day_values)
      }
    })
    
    # En resultsServer - Actualización de opciones de arena
    observe({
      req(values$processed_data)
      
      # Actualizar opciones de arena - CORREGIR la estructura de datos
      if (!is.null(values$processed_data$factors) && "_Arena" %in% names(values$processed_data$factors)) {
        arenas <- unique(values$processed_data$factors$`_Arena`)
        arenas <- gsub("\\.txt$", "", arenas)  # Eliminar extensión .txt si existe
        arenas <- arenas[!is.na(arenas) & arenas != ""]  # Eliminar valores vacíos
        updateSelectizeInput(session, "arena_filter", 
                            choices = c("Todas" = "", arenas),
                            selected = "")
      }
    })
    
    # FUNCIÓN DE FILTRADO GLOBAL MEJORADA
    filtered_data <- reactive({
      req(values$processed_data)
      data <- values$processed_data

      if (is.null(data$factors)) {
        showNotification("Error: estructura de datos incorrecta", type = "error")
        return(NULL)
      }

      # Preservar toda la estructura y luego filtrar
      result_data <- data
      filtered_factors <- data$factors

      # Probe: aceptar FALSE o 0 como entrenamiento
      if (input$probe_filter == "Solo entrenamiento (Probe = FALSE)") {
        if ("Probe" %in% names(filtered_factors)) {
          filtered_factors <- filtered_factors[filtered_factors$Probe == FALSE | filtered_factors$Probe == 0, , drop = FALSE]
        }
      } else if (input$probe_filter == "Solo pruebas (Probe = TRUE)") {
        if ("Probe" %in% names(filtered_factors)) {
          filtered_factors <- filtered_factors[filtered_factors$Probe == TRUE, , drop = FALSE]
        }
      }

      # Arena: compatibilidad con/sin .txt
      if (!is.null(input$arena_filter) && length(input$arena_filter) > 0 && input$arena_filter[1] != "") {
        if ("_Arena" %in% names(filtered_factors)) {
          sel <- unique(input$arena_filter)
          wanted <- unique(c(sel, paste0(sel, ".txt"), sub("\\.txt$", "", sel)))
          filtered_factors <- filtered_factors[filtered_factors$`_Arena` %in% wanted, , drop = FALSE]
        }
      }

      if (nrow(filtered_factors) == 0) {
        showNotification("No hay datos después del filtrado. Revisa los filtros.", type = "warning")
        return(NULL)
      }

      # Track IDs restantes
      remaining_ids <- unique(filtered_factors$`_TrackID`)

      # Asignar factors filtrados
      result_data$factors <- filtered_factors

      # Filtrar metrics si existen
      if (!is.null(result_data$metrics) && "_TrackID" %in% names(result_data$metrics)) {
        result_data$metrics <- result_data$metrics[result_data$metrics$`_TrackID` %in% remaining_ids, , drop = FALSE]
      }

      # Filtrar paths si existen
      if (!is.null(result_data$paths)) {
        if (is.list(result_data$paths)) {
          # Si paths tiene nombres
          if (!is.null(names(result_data$paths))) {
            result_data$paths <- result_data$paths[names(result_data$paths) %in% remaining_ids]
          } else {
            # Intentar extraer _TrackID de cada path
            valid_paths <- list()
            for (i in seq_along(result_data$paths)) {
              p <- result_data$paths[[i]]
              id <- NULL
              if (!is.null(p$`_TrackID`)) {
                id <- p$`_TrackID`
              } else if (!is.null(attr(p, "_TrackID"))) {
                id <- attr(p, "_TrackID")
              }
              if (!is.null(id) && id %in% remaining_ids) {
                valid_paths[[length(valid_paths) + 1]] <- p
              }
            }
            result_data$paths <- valid_paths
          }
        }
      }

      # Centralizar los filtros para que otros módulos los usen
      values$filters <- list(
        probe = input$probe_filter,
        arenas = input$arena_filter
      )

      return(result_data)
    })
    
    # Generar mapas de densidad
    observeEvent(input$generate_density_maps, {
      req(input$density_grouping)

      tryCatch({
        showNotification("Generando mapas de densidad...", type = "message")
        
        # Obtener datos filtrados
        filtered_dataset <- filtered_data()
        if (is.null(filtered_dataset)) {
          showNotification("No hay datos disponibles después del filtrado", type = "warning")
          return()
        }
        
        # Verificar existencia de paths (debug)
        if (is.null(filtered_dataset$paths)) {
          showNotification("No se encontraron paths en los datos filtrados", type = "error")
          return()
        }
        
        # Verificar si paths está vacío (debug)
        if (is.list(filtered_dataset$paths) && length(filtered_dataset$paths) == 0) {
          showNotification("La lista de paths está vacía", type = "error")
          return()
        }
        
        # Imprimir información para debug
        cat("Paths encontrados:", length(filtered_dataset$paths), "\n")
        cat("Estructura de paths:", class(filtered_dataset$paths), "\n")
        if (is.list(filtered_dataset$paths)) {
          cat("Nombres de paths:", head(names(filtered_dataset$paths)), "\n")
        }
        
        # Intentar generar los mapas
        plots_result <- create_density_maps(
          experiment_data = filtered_dataset,
          grouping_var = input$density_grouping,
          color_palette = input$color_palette,
          resolution = input$resolution,
          color_levels = input$color_levels,
          show_legend = input$show_legend,
          overlay_goals = FALSE  # No dibujar old.goal
        )
        
        # Verificar resultado
        if (is.null(plots_result)) {
          showNotification("Error: resultado NULL en create_density_maps", type = "error")
          return()
        } else if (is.character(plots_result)) {
          showNotification(paste("Error:", plots_result), type = "error")
          return()
        }
        
        # Todo bien, actualizar los mapas
        density_maps(plots_result)
        values$density_plots <- plots_result
        
        showNotification("Mapas de densidad generados correctamente", type = "message")
      }, error = function(e) {
        showNotification(paste("Error generando mapas:", e$message), type = "error")
        print(paste("Debug - Error completo:", as.character(e)))
      })
    })    # Generar análisis de estrategias
    observeEvent(input$generate_strategy_analysis, {
      req(values$strategies, values$processed_data, input$strategy_grouping, input$strategy_days)
      
      tryCatch({
        showNotification("Generando análisis de estrategias...", type = "message")
        
        # Verificar datos filtrados
        filtered_dataset <- filtered_data()
        if (is.null(filtered_dataset)) {
          showNotification("No hay datos disponibles después del filtrado", type = "warning")
          return()
        }
        
        # Filtrar estrategias basado en los Track_IDs que quedaron
        remaining_track_ids <- unique(filtered_dataset$factors$`_TrackID`)
        filtered_strategies <- values$strategies
        
        # Verificar que las estrategias tienen la estructura correcta
        if (!is.null(filtered_strategies$calls)) {
          available_tracks <- rownames(filtered_strategies$calls)
          valid_tracks <- intersect(remaining_track_ids, available_tracks)
          
          if (length(valid_tracks) == 0) {
            showNotification("No hay estrategias disponibles para los datos filtrados", type = "warning")
            return()
          }
          
          filtered_strategies$calls <- filtered_strategies$calls[valid_tracks, , drop = FALSE]
        }
        
        results <- create_strategy_analysis(
          strategies_data = filtered_strategies,
          experiment_data = filtered_dataset,
          grouping_var = input$strategy_grouping,
          days_filter = input$strategy_days,
          show_individual = input$show_individual_strategies
        )
        
        strategy_plots(results$plots)
        statistical_results(results$stats)
        values$strategy_plots <- results$plots
        values$statistical_results <- results$stats
        showNotification("Análisis de estrategias generado correctamente", type = "message")
      }, error = function(e) {
        showNotification(paste("Error generando análisis:", e$message), type = "error")
        print(paste("Debug - Error en estrategias:", e))  # Para debugging
      })
    })
    
    # Generar análisis completo
    observeEvent(input$generate_analysis, {
      req(values$analysis_config, input$analysis_variable, input$analysis_grouping)
      
      tryCatch({
        showNotification("Generando análisis completo...", type = "message")
        
        # Usar los datos exportados filtrados
        filtered_results <- filtered_data()
        
        if (!is.null(filtered_results)) {
          plot_obj <- create_comprehensive_analysis(
            results_data = filtered_results,
            variable = input$analysis_variable,
            grouping_var = input$analysis_grouping,
            days_filter = input$analysis_days,
            probe_filter = input$probe_filter,
            arena_filter = input$arena_filter,
            show_error_bars = input$show_error_bars,
            log_transform = input$log_transform
          )
          
          analysis_plots(plot_obj)
          showNotification("Análisis completo generado correctamente", type = "message")
        } else {
          showNotification("No hay datos después del filtrado", type = "warning")
        }
        
      }, error = function(e) {
        showNotification(paste("Error generando análisis:", e$message), type = "error")
      })
    })
    
    # Ejecutar análisis estadísticos
    observeEvent(input$run_statistics, {
      req(values$analysis_config, input$analysis_variable, input$analysis_grouping, input$stat_test)
      
      tryCatch({
        showNotification("Ejecutando análisis estadístico...", type = "message")
        
        if (!is.null(values$analysis_config$results_export)) {
          # Aplicar filtros de Probe y Arena a los datos de estadística
          data_for_stats <- values$analysis_config$results_export
          
          # CAMBIO: Filtrar por Probe como en filtered_data()
          if (input$probe_filter == "Solo entrenamiento (Probe = FALSE)") {
            data_for_stats <- data_for_stats[data_for_stats$Probe == FALSE, ]
          } else if (input$probe_filter == "Solo pruebas (Probe = TRUE)") {
            data_for_stats <- data_for_stats[data_for_stats$Probe == TRUE, ]
          }
          
          # CAMBIO: Filtrar por Arena como en filtered_data()
          if (!is.null(input$arena_filter) && length(input$arena_filter) > 0 && input$arena_filter[1] != "") {
            selected_arenas <- input$arena_filter
            # Mejor método para filtrar por arena: usar %in% en vez de grepl
            data_for_stats <- data_for_stats[data_for_stats$`_Arena` %in% 
                                      paste0(selected_arenas, ifelse(grepl("\\.txt$", selected_arenas), "", ".txt")), ]
          }
          
          # Filtrar por días (ya estaba implementado)
          if (!is.null(input$analysis_days)) {
            day_col <- names(data_for_stats)[grepl("^_Day", names(data_for_stats))][1]
            if (!is.null(day_col)) {
              data_for_stats <- data_for_stats[data_for_stats[[day_col]] %in% input$analysis_days, ]
            }
          }
          
          # CAMBIO: Añadir los filtros a la función de análisis estadístico
          stats_result <- perform_statistical_analysis(
            data = data_for_stats,
            variable = input$analysis_variable,
            grouping_var = input$analysis_grouping,
            test_type = input$stat_test,
            probe_filter = input$probe_filter,     # NUEVO: pasar filtro de probe
            arena_filter = input$arena_filter      # NUEVO: pasar filtro de arena
          )
          
          stats_results(stats_result)
          
          showNotification("Análisis estadístico completado", type = "message")
        } else {
          showNotification("No hay datos exportados disponibles", type = "warning")
        }
      }, error = function(e) {
        showNotification(paste("Error en análisis estadístico:", e$message), type = "error")
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
    
    output$analysis_plots_ready <- reactive({
      !is.null(analysis_plots())
    })
    outputOptions(output, "analysis_plots_ready", suspendWhenHidden = FALSE)
    
    output$stats_results_available <- reactive({
      !is.null(stats_results())
    })
    outputOptions(output, "stats_results_available", suspendWhenHidden = FALSE)
    
    # Renderizar plots
    output$density_plots <- renderPlot({
      req(density_maps())
      obj <- density_maps()
      if (is.function(obj)) {
        obj()
      } else {
        print(obj)
      }
    })
    
    output$strategy_plots <- renderPlot({
      req(strategy_plots())
      strategy_plots()
    })
    
    output$analysis_plots <- renderPlot({
      req(analysis_plots())
      analysis_plots()
    })
    
    # Tablas de datos
    output$tracks_summary <- DT::renderDataTable({
      req(values$processed_data)
      
      # Helper para devolver columna si existe o NA del largo correcto
      get_col <- function(df, colname) {
        if (colname %in% names(df)) return(df[[colname]])
        rep(NA, nrow(df))
      }
      f <- values$processed_data$factors
      summary_data <- data.frame(
        Track_ID = get_col(f, "_TrackID"),
        Subject_ID = get_col(f, "_TargetID"),
        Day = get_col(f, "_Day"),
        Trial = get_col(f, "_Trial")
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
    
    output$statistical_results <- renderPrint({
      req(stats_results())
      res <- stats_results()
      if (!is.null(res$result)) {
        cat("Prueba:", res$test, "\n")
        if (!is.null(res$formula)) {
          cat("Fórmula:", deparse(res$formula), "\n")
        }
        if (!is.null(res$family)) {
          cat("Familia:", res$family, "\n")
        }
        cat("\nResultado principal:\n")
        print(res$result)
        if (!is.null(res$lrt)) {
          cat("\nComparación LRT (modelo sin interacción vs con interacción):\n")
          print(res$lrt)
        }
      } else {
        "Análisis estadístico no disponible"
      }
    })
    
    output$exported_results_table <- DT::renderDataTable({
      req(values$analysis_config)
      
      if (!is.null(values$analysis_config$results_export)) {
        DT::datatable(
          values$analysis_config$results_export,
          options = list(
            pageLength = 15,
            scrollX = TRUE,
            dom = 'Blfrtip',
            buttons = c('copy', 'csv', 'excel')
          ),
          class = 'cell-border stripe hover',
          extensions = 'Buttons'
        ) %>%
          DT::formatRound(columns = which(sapply(values$analysis_config$results_export, is.numeric)), digits = 3)
      }
    })
    
    # Reemplazar el único output$filter_status con cuatro outputs diferentes:
    
    # Para mapas de densidad
    output$density_filter_status <- renderUI({
      create_filter_status_ui(input$probe_filter, input$arena_filter)
    })
    
    # Para estrategias de búsqueda
    output$strategy_filter_status <- renderUI({
      create_filter_status_ui(input$probe_filter, input$arena_filter)
    })
    
    # Para datos procesados
    output$data_filter_status <- renderUI({
      create_filter_status_ui(input$probe_filter, input$arena_filter)
    })
    
    # Para análisis completo
    output$analysis_filter_status <- renderUI({
      create_filter_status_ui(input$probe_filter, input$arena_filter)
    })
    
    # Función auxiliar para crear el UI de estado de filtros
    create_filter_status_ui <- function(probe_filter, arena_filter) {
      filter_text <- NULL
      
      if (!is.null(probe_filter) && probe_filter != "Todos") {
        filter_text <- c(filter_text, paste("Probe:", probe_filter))
      }
      
      if (!is.null(arena_filter) && length(arena_filter) > 0 && arena_filter[1] != "") {
        filter_text <- c(filter_text, paste("Arenas:", paste(arena_filter, collapse=", ")))
      }
      
      if (length(filter_text) > 0) {
        tags$div(
          class = "alert alert-success",
          icon("filter"), 
          "Filtros activos: ", 
          paste(filter_text, collapse="; ")
        )
      } else {
        tags$div(
          class = "alert alert-info",
          icon("info-circle"),
          "No hay filtros activos. Mostrando todos los datos."
        )
      }
    }
  })
}
