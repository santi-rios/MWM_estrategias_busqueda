# Módulo para procesamiento y análisis
# Ejecuta el análisis con Rtrack y genera las métricas

analysisUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "⚙️ Configuración de Análisis",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        
        h4("Parámetros de Procesamiento"),
        
        fluidRow(
          column(6,
            checkboxInput(ns("parallel_processing"),
                         "Procesamiento paralelo",
                         value = TRUE)
          ),
          column(6,
            conditionalPanel(
              condition = paste0("input['", ns("parallel_processing"), "']"),
              numericInput(ns("num_threads"),
                          "Número de hilos:",
                          value = 1,
                          min = 1,
                          max = 1,
                          step = 1)
            )
          )
        ),
        
        hr(),
        
        h4("Configuración de Estrategias"),
        
  fluidRow(
          column(6,
            checkboxInput(ns("threshold_strategies"),
                         "Aplicar umbral de confianza",
                         value = FALSE)
          ),
          column(6,
            conditionalPanel(
              condition = paste0("input['", ns("threshold_strategies"), "']"),
              numericInput(ns("confidence_threshold"),
                          "Umbral de confianza:",
                          value = 0.4,
                          min = 0,
                          max = 1,
                          step = 0.1)
            )
          )
        ),
  tags$p(style="font-size:12px; color:#555; margin-top:-8px;",
         "El clasificador Rtrack devuelve una confianza por track. Usa el umbral para ocultar llamadas poco claras (p.ej., 0.4)."),
        
        hr(),
        
        h4("Configuración de Grupos"),
        p("Define cómo agrupar tus datos para el análisis."),
        tags$ul(style="font-size:12px; color:#555;",
          tags$li(tags$b("Agrupar por:"), " factor principal para comparar (p.ej., Grupo, Sexo)."),
          tags$li(tags$b("Tratamiento:"), " factor clave que se guarda para análisis/exports (puede coincidir con 'Agrupar por')."),
          tags$li(tags$b("Día:"), " variable temporal (p.ej., _Day) para análisis longitudinal.")
        ),
        
        conditionalPanel(
          condition = paste0("output['", ns("experiment_ready"), "']"),
          
          selectInput(ns("grouping_variables"),
                     "Variables de agrupación:",
                     choices = NULL,
                     multiple = TRUE),
          
          selectInput(ns("treatment_variable"),
                     "Variable de tratamiento:",
                     choices = NULL),
          
          selectInput(ns("day_variable"),
                     "Variable de día/tiempo:",
                     choices = NULL)
        ),
        
        br(),
        
        div(
          style = "text-align: center;",
          actionButton(ns("start_analysis"),
                      "🚀 Iniciar Análisis",
                      class = "btn-success btn-lg",
                      style = "margin: 10px;")
        )
      ),
      
      box(
        title = "📊 Estado del Análisis",
        status = "info",
        solidHeader = TRUE,
        width = 6,
        
        h4("Progreso"),
        
        conditionalPanel(
          condition = paste0("!output['", ns("analysis_running"), "']"),
          div(
            style = "text-align: center; color: #666; padding: 40px;",
            icon("play-circle", style = "font-size: 48px;"),
            h4("Listo para procesar"),
            p("Configura los parámetros y presiona 'Iniciar Análisis'")
          )
        ),
        
        conditionalPanel(
          condition = paste0("output['", ns("analysis_running"), "']"),
          div(
            style = "text-align: center; padding: 20px;",
            div(class = "spinner-border text-primary", role = "status"),
            h4("Procesando..."),
            br(),
            textOutput(ns("progress_text"))
          )
        ),
        
        conditionalPanel(
          condition = paste0("output['", ns("analysis_complete"), "']"),
          div(
            style = "text-align: center; color: green; padding: 20px;",
            icon("check-circle", style = "font-size: 48px; color: green;"),
            h4("✅ Análisis Completado"),
            br(),
            actionButton(ns("view_results"),
                        "👀 Ver Resultados",
                        class = "btn-primary btn-lg")
          )
        ),
        
        hr(),
        
        h5("Log de Procesamiento"),
        div(
          style = "height: 200px; overflow-y: auto; background: #f8f9fa; padding: 10px; border: 1px solid #ddd;",
          verbatimTextOutput(ns("processing_log"))
        )
      )
    ),
    
    fluidRow(
      column(12,
        conditionalPanel(
          condition = paste0("output['", ns("analysis_complete"), "']"),
          box(
            title = "📈 Resumen de Resultados",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            
            fluidRow(
              column(3,
                div(
                  style = "text-align: center; padding: 20px; background: #e3f2fd; border-radius: 5px;",
                  h3(textOutput(ns("total_tracks")), style = "color: #1976d2; margin: 0;"),
                  p("Tracks procesados", style = "margin: 5px 0 0 0;")
                )
              ),
              column(3,
                div(
                  style = "text-align: center; padding: 20px; background: #f3e5f5; border-radius: 5px;",
                  h3(textOutput(ns("total_subjects")), style = "color: #7b1fa2; margin: 0;"),
                  p("Sujetos analizados", style = "margin: 5px 0 0 0;")
                )
              ),
              column(3,
                div(
                  style = "text-align: center; padding: 20px; background: #e8f5e8; border-radius: 5px;",
                  h3(textOutput(ns("total_days")), style = "color: #388e3c; margin: 0;"),
                  p("Días de experimento", style = "margin: 5px 0 0 0;")
                )
              ),
              column(3,
                div(
                  style = "text-align: center; padding: 20px; background: #fff3e0; border-radius: 5px;",
                  h3(textOutput(ns("strategy_accuracy")), style = "color: #f57c00; margin: 0;"),
                  p("Confianza promedio", style = "margin: 5px 0 0 0;")
                )
              )
            )
          )
        )
      )
    )
  )
}

analysisServer <- function(id, values, parent_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Variables reactivas
    analysis_running <- reactiveVal(FALSE)
    analysis_complete <- reactiveVal(FALSE)
    processing_log <- reactiveVal("")
    results_data <- reactiveVal(NULL)
    
    # Función para agregar al log
    add_to_log <- function(message) {
      timestamp <- format(Sys.time(), "%H:%M:%S")
      new_message <- paste0("[", timestamp, "] ", message)
      current_log <- processing_log()
      processing_log(paste(current_log, new_message, sep = "\n"))
    }
    
    # Verificar si el experimento está listo
    output$experiment_ready <- reactive({
      !is.null(values$experiment_data) && !is.null(values$arena_config)
    })
    outputOptions(output, "experiment_ready", suspendWhenHidden = FALSE)
    
    # Actualizar opciones de agrupación cuando los datos están listos
    observe({
      req(values$experiment_data)
      
      # Obtener nombres de columnas del experimento
      col_names <- names(values$experiment_data)
      
      # Filtrar columnas que no empiecen con _
      grouping_choices <- col_names[!grepl("^_", col_names)]
      system_choices <- col_names[grepl("^_", col_names)]
      
      updateSelectInput(session, "grouping_variables",
                       choices = grouping_choices)
      
      updateSelectInput(session, "treatment_variable",
                       choices = grouping_choices)
      
      updateSelectInput(session, "day_variable",
                       choices = system_choices,
                       selected = "_Day")
    })
    
    # Iniciar análisis
    observeEvent(input$start_analysis, {
      req(values$experiment_data, values$arena_config, values$track_files_data)
      
      analysis_running(TRUE)
      analysis_complete(FALSE)
      processing_log("")
      
      add_to_log("Iniciando análisis de MWM...")
      
      tryCatch({
        # Configurar procesamiento
        threads <- if (input$parallel_processing) input$num_threads else 1
        add_to_log(paste("Configurando procesamiento con", ifelse(threads == 0, "automático", threads), "hilos"))
        
        # Procesar experimento con Rtrack
        add_to_log("Leyendo archivos de experimento...")

        # Copiar los datos para no modificar el original reactivo
        temp_experiment_data <- values$experiment_data
        
        # Asegurarnos de que los archivos de arena requeridos existen en project_dir
        arenas_needed <- unique(na.omit(temp_experiment_data$`_Arena`))
        if (length(arenas_needed) == 0) {
          stop("El experimento no especifica ninguna arena en la columna _Arena")
        }

        # Crear un directorio temporal para los archivos de análisis
        temp_dir <- tempfile(pattern = "mwm_analysis_")
        dir.create(temp_dir)
        
        # Crear archivo temporal del experimento en el directorio temporal
        temp_exp_file <- file.path(temp_dir, "experiment_data.xlsx")
        writexl::write_xlsx(temp_experiment_data, temp_exp_file)
        
        # Copiar todos los archivos de arena requeridos al directorio del proyecto
        add_to_log("Preparando archivos de arena...")
        if (is.null(values$arena_config) || is.null(values$arena_config$files)) {
          stop("No hay archivos de arena disponibles. Genera o sube los .txt en la pestaña 'Configurar Arena'.")
        }
        arena_files_map <- values$arena_config$files
        # Normalizar nombres requeridos a 'BaseName.txt'
        arenas_required_files <- vapply(arenas_needed, function(a) {
          nm <- if (grepl("\\.txt$", a)) a else paste0(a, ".txt")
          sub("^.*/", "", nm)
        }, character(1))
        # Verificar disponibilidad y copiar
        for (fname in arenas_required_files) {
          src <- arena_files_map[[fname]]
          if (is.null(src) || !file.exists(src)) {
            stop(paste0("Falta el archivo de arena requerido: ", fname, ". Genera o sube este archivo."))
          }
          file.copy(src, file.path(temp_dir, fname), overwrite = TRUE)
        }
        add_to_log(paste("Arenas listas:", paste(arenas_required_files, collapse = ", ")))        
        
        add_to_log("Procesando tracks con Rtrack...")
        
        # Llamar a la función de procesamiento con mejor manejo de errores
        experiment_result <- tryCatch({
          process_mwm_experiment(
            experiment_file = temp_exp_file,
            data_dir = values$track_files_data$directory,
            project_dir = temp_dir,
            threads = threads
          )
        }, error = function(e) {
          # Proporcionar mensaje más específico según el tipo de error
          error_msg <- e$message
          
          if (grepl("argument is of length zero", error_msg)) {
            error_msg <- paste("Error de datos vacíos. Posibles causas:",
                              "- Archivos de tracks vacíos",
                              "- Nombres de archivos no coinciden con el experimento",
                              "- Formato de datos incorrecto",
                              "- Datos faltantes en archivos de tracks", sep = "\n")
          } else if (grepl("cannot open file", error_msg)) {
            error_msg <- paste("Error al acceder a archivos:",
                              "- Verificar que todos los archivos de tracks estén cargados",
                              "- Comprobar que los nombres coincidan exactamente", sep = "\n")
          } else if (grepl("Archivos de tracks faltantes", error_msg)) {
            error_msg <- paste("Algunos archivos de tracks no están disponibles:",
                              "- La aplicación continuará con los archivos disponibles",
                              "- Si deseas usar todos los tracks, carga los archivos faltantes", sep = "\n")
          }
          
          stop(error_msg)
        })
        
        add_to_log("Calculando estrategias de búsqueda...")
        
        # Calcular estrategias
        strategies <- Rtrack::call_strategy(experiment_result)
        
        # Aplicar umbral si está activado
        if (input$threshold_strategies) {
          add_to_log(paste("Aplicando umbral de confianza:", input$confidence_threshold))
          strategies <- Rtrack::threshold_strategies(strategies, input$confidence_threshold)
        }
        
        add_to_log("Procesando datos para análisis estadístico...")
        
        # Procesar datos para análisis
        processed_data <- process_strategy_data(
          experiment_result,
          strategies,
          grouping_vars = input$grouping_variables,
          treatment_var = input$treatment_variable,
          day_var = input$day_variable
        )
        
        # Guardar resultados
        results_data(list(
          experiment = experiment_result,
          strategies = strategies,
          processed_data = processed_data,
          config = list(
            grouping_vars = input$grouping_variables,
            treatment_var = input$treatment_variable,
            day_var = input$day_variable,
            threshold = input$threshold_strategies,
            confidence = input$confidence_threshold
          )
        ))
        
        # Pasar resultados a values globales
        values$processed_data <- experiment_result
        values$strategies <- strategies
        values$analysis_config <- processed_data
        values$processing_complete <- TRUE
        
        add_to_log("✅ Análisis completado exitosamente!")
        analysis_complete(TRUE)
        
  showNotification("Análisis completado correctamente", type = "message", duration = 5)
        
      }, error = function(e) {
        add_to_log(paste("❌ Error:", e$message))
        showNotification(paste("Error en análisis:", e$message), type = "error", duration = 10)
      }, finally = {
        analysis_running(FALSE)
      })
    })
    
    # Outputs de estado
    output$analysis_running <- reactive({
      analysis_running()
    })
    outputOptions(output, "analysis_running", suspendWhenHidden = FALSE)
    
    output$analysis_complete <- reactive({
      analysis_complete()
    })
    outputOptions(output, "analysis_complete", suspendWhenHidden = FALSE)
    
    output$processing_log <- renderText({
      processing_log()
    })
    
    output$progress_text <- renderText({
      if (analysis_running()) {
        "Procesando datos..."
      }
    })
    
    # Resúmenes de resultados
    output$total_tracks <- renderText({
      if (!is.null(results_data())) {
        length(results_data()$experiment$metrics)
      } else {
        "0"
      }
    })
    
    output$total_subjects <- renderText({
      if (!is.null(results_data())) {
        length(unique(results_data()$experiment$factors$`_TargetID`))
      } else {
        "0"
      }
    })
    
    output$total_days <- renderText({
      if (!is.null(results_data())) {
        length(unique(results_data()$experiment$factors$`_Day`))
      } else {
        "0"
      }
    })
    
    output$strategy_accuracy <- renderText({
      if (!is.null(results_data())) {
        avg_confidence <- mean(results_data()$strategies$calls$confidence, na.rm = TRUE)
        paste0(round(avg_confidence * 100, 1), "%")
      } else {
        "0%"
      }
    })
    
    # Botón para ver resultados
    observeEvent(input$view_results, {
      if (!is.null(parent_session)) {
        shinydashboard::updateTabItems(parent_session, "tabs", "results")
      } else {
        shinydashboard::updateTabItems(session$parent, "tabs", "results")
      }
    })
  })
}
