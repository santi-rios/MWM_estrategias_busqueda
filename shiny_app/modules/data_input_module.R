# Módulo para carga de datos
# Maneja la carga del archivo de experimento y archivos de tracks

dataInputUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Sección de datos de ejemplo
    fluidRow(
      box(
        title = "🎓 Datos de Ejemplo para Aprender",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        
        div(
          style = "background: #e3f2fd; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
          h5(icon("graduation-cap"), " Tutorial y Datos de Ejemplo"),
          p("Esta aplicación está basada en el paquete", tags$strong("Rtrack"), "desarrollado por Rupert Overall."),
          p("Para aprender más sobre el análisis de MWM, visita el tutorial oficial:"),
          tags$a(
            href = "https://rupertoverall.net/Rtrack/articles/Rtrack_MWM_analysis.html",
            target = "_blank",
            class = "btn btn-link",
            icon("external-link-alt"), " Tutorial Original de Rtrack"
          )
        ),
        
        fluidRow(
          column(6,
            div(
              style = "text-align: center;",
              h5("🚀 Opción 1: Carga Automática"),
              p("Descarga y carga automáticamente los datos de ejemplo desde GitHub (múltiples días)."),
              actionButton(ns("load_example_data"),
                          "📦 Cargar Datos de Ejemplo",
                          class = "btn-success btn-lg",
                          style = "margin: 10px;"),
              br(),
              conditionalPanel(
                condition = paste0("output['", ns("example_loading"), "']"),
                div(
                  style = "margin-top: 10px;",
                  div(class = "spinner-border spinner-border-sm text-primary", role = "status"),
                  span(" Descargando datos...", style = "margin-left: 10px;")
                )
              )
            )
          ),
          column(6,
            div(
              style = "text-align: center;",
              h5("📥 Opción 2: Descarga Manual"),
              p("Descarga los archivos manualmente desde GitHub y súbelos usando las opciones de abajo."),
              tags$a(
                href = "https://github.com/santi-rios/MWM_estrategias_busqueda/tree/main/data/example_data/example_data",
                target = "_blank",
                class = "btn btn-outline-primary",
                icon("github"), " Ver Datos en GitHub"
              ),
              br(), br(),
              tags$small(
                style = "color: #666;",
                "Descarga: Experiment.xlsx, Arena_NE.txt, Arena_SW.txt y los archivos de Data/."
              )
            )
          )
        ),
        
        conditionalPanel(
          condition = paste0("output['", ns("example_loaded"), "']"),
          div(
            style = "background: #d4edda; padding: 10px; border-radius: 5px; margin-top: 15px;",
            icon("check-circle", style = "color: green;"),
            span(" Datos de ejemplo cargados correctamente. ¡Puedes continuar con la configuración de la arena!", 
                 style = "color: green; margin-left: 5px;")
          )
        )
      )
    ),
    
    # Separador
    fluidRow(
      column(12,
        hr(),
        h4("📁 O Carga Tus Propios Datos", style = "text-align: center; color: #666;"),
        hr()
      )
    ),
    
    # Sección original de carga de datos
    fluidRow(
      box(
        title = "1️⃣ Archivo de Experimento (.xlsx)",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        
        p("Sube tu archivo Excel con la información del experimento."),
        
        fileInput(ns("experiment_file"),
                 "Seleccionar archivo .xlsx:",
                 accept = c(".xlsx", ".xls"),
                 placeholder = "No hay archivo seleccionado"),
        
        conditionalPanel(
          condition = paste0("output['", ns("experiment_uploaded"), "']"),
          
          h5("📋 Vista Previa de Datos"),
          p("Primeras 5 filas del archivo cargado:"),
          
          div(
            style = "max-height: 300px; overflow-y: auto; border: 1px solid #ddd; padding: 10px;",
            DT::dataTableOutput(ns("experiment_preview"))
          ),
          
          br(),
          
          div(
            style = "background: #d4edda; padding: 10px; border-radius: 5px;",
            icon("check", style = "color: green;"),
            span(" Archivo cargado correctamente", style = "color: green; margin-left: 5px;")
          )
        )
      ),
      
      box(
        title = "2️⃣ Archivos de Tracks",
        status = "warning", 
        solidHeader = TRUE,
        width = 6,
        
        p("Sube todos los archivos de tracks (.txt o .csv) de una vez."),
        
        fileInput(ns("track_files"),
                 "Seleccionar archivos .txt/.csv:",
                 accept = c(".txt", ".csv"),
                 multiple = TRUE,
                 placeholder = "No hay archivos seleccionados"),
        
        conditionalPanel(
          condition = paste0("output['", ns("tracks_uploaded"), "']"),
          
          h5("📁 Archivos Cargados"),
          div(
            style = "max-height: 200px; overflow-y: auto; background: #f8f9fa; padding: 10px; border: 1px solid #ddd;",
            verbatimTextOutput(ns("track_files_list"))
          ),
          
          br(),
          
          div(
            style = "background: #d4edda; padding: 10px; border-radius: 5px;",
            icon("check", style = "color: green;"),
            textOutput(ns("tracks_status"))
          )
        )
      )
    ),
    
    # Estado general
    fluidRow(
      box(
        title = "📊 Estado de Carga",
        status = "success",
        solidHeader = TRUE,
        width = 12,
        
        conditionalPanel(
          condition = paste0("!output['", ns("data_ready"), "']"),
          div(
            style = "text-align: center; color: #666; padding: 20px;",
            icon("upload", style = "font-size: 48px;"),
            h4("Datos pendientes"),
            p("Carga el archivo de experimento y los archivos de tracks para continuar.")
          )
        ),
        
        conditionalPanel(
          condition = paste0("output['", ns("data_ready"), "']"),
          div(
            style = "text-align: center; color: green; padding: 20px;",
            icon("check-circle", style = "font-size: 48px; color: green;"),
            h4("✅ Datos listos para procesar"),
            p("Continúa con la configuración de la arena."),
            br(),
            actionButton(ns("go_to_arena"),
                        "🎯 Configurar Arena",
                        class = "btn-primary btn-lg")
          )
        )
      )
    )
  )
}

dataInputServer <- function(id, values, parent_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Variables reactivas
    experiment_data <- reactiveVal(NULL)
    track_files_data <- reactiveVal(NULL)
    example_loading <- reactiveVal(FALSE)
    example_loaded <- reactiveVal(FALSE)
    
    # Función para cargar datos de ejemplo desde GitHub
    observeEvent(input$load_example_data, {
      example_loading(TRUE)
      
      tryCatch({
        # URLs base de GitHub (raw content)
        base_url <- "https://raw.githubusercontent.com/santi-rios/MWM_estrategias_busqueda/main/data/example_data/example_data/"
        
        # Lista de archivos necesarios con URLs correctas
        files_to_download <- list(
          experiment = list(
            url = paste0(base_url, "Experiment.xlsx"),
            filename = "Experiment.xlsx"
          ),
          arenas = list(
            list(url = paste0(base_url, "Arena_NE.txt"), filename = "Arena_NE.txt"),
            list(url = paste0(base_url, "Arena_SW.txt"), filename = "Arena_SW.txt")
          )
        )
        
        # Definir rangos de tracks para múltiples días
        track_ranges <- list(
          day1 = 1:30,    # Día 1: tracks 1-30
          day2 = 31:60,   # Día 2: tracks 31-60
          day3 = 61:90    # Día 3: tracks 61-90 (si existen)
        )
        
        # Crear directorio temporal
        temp_dir <- tempfile(pattern = "mwm_example_")
        dir.create(temp_dir)
        
        showNotification("Descargando archivo de experimento...", type = "message", duration = 3)
        
        # Descargar archivo de experimento
        experiment_path <- file.path(temp_dir, files_to_download$experiment$filename)
        download.file(files_to_download$experiment$url, experiment_path, mode = "wb", quiet = TRUE)
        
        # Verificar que se descargó correctamente
        if (!file.exists(experiment_path) || file.size(experiment_path) == 0) {
          stop("No se pudo descargar el archivo de experimento")
        }
        
        # Leer el archivo de experimento
        exp_data <- readxl::read_excel(experiment_path)
        experiment_data(exp_data)
        
        showNotification("Descargando archivos de arena...", type = "message", duration = 3)
        
        # Descargar archivos de arena
        arena_files <- list()
        for (arena in files_to_download$arenas) {
          arena_path <- file.path(temp_dir, arena$filename)
          tryCatch({
            download.file(arena$url, arena_path, mode = "wb", quiet = TRUE)
            if (file.exists(arena_path) && file.size(arena_path) > 0) {
              arena_files[[arena$filename]] <- arena_path
            }
          }, error = function(e) {
            warning(paste("No se pudo descargar:", arena$filename))
          })
        }
        
        showNotification("Descargando archivos de tracks (múltiples días)...", type = "message", duration = 8)
        
        # Descargar archivos de tracks de múltiples días
        track_paths <- character()
        track_names <- character()
        tracks_downloaded_by_day <- list()
        
        for (day_name in names(track_ranges)) {
          tracks_for_day <- character()
          track_numbers <- track_ranges[[day_name]]
          
          for (i in track_numbers) {
            track_url <- paste0(base_url, "Data/Track_", i, ".csv")
            track_filename <- paste0("Track_", i, ".csv")
            track_path <- file.path(temp_dir, track_filename)
            
            tryCatch({
              download.file(track_url, track_path, mode = "wb", quiet = TRUE)
              
              if (file.exists(track_path) && file.size(track_path) > 0) {
                track_paths <- c(track_paths, track_path)
                track_names <- c(track_names, track_filename)
                tracks_for_day <- c(tracks_for_day, track_filename)
              } else {
                # Si no se puede descargar, eliminar archivo vacío
                if (file.exists(track_path)) {
                  file.remove(track_path)
                }
              }
            }, error = function(e) {
              # Silencioso para archivos que no existen
            })
          }
          
          tracks_downloaded_by_day[[day_name]] <- tracks_for_day
          
          # Mostrar progreso por día
          if (length(tracks_for_day) > 0) {
            showNotification(
              paste("Día", gsub("day", "", day_name), ":", length(tracks_for_day), "tracks descargados"),
              type = "message",
              duration = 2
            )
          }
        }
        
        if (length(track_paths) == 0) {
          stop("No se pudieron descargar archivos de tracks")
        }
        
        names(track_paths) <- track_names
        
        # Guardar información de tracks
        track_files_data(list(
          files = track_paths,
          directory = temp_dir,
          names = track_names,
          days_info = tracks_downloaded_by_day
        ))
        
        # Actualizar valores globales
        values$experiment_data <- exp_data
        values$track_files_data <- list(
          files = track_paths,
          directory = temp_dir,
          names = track_names,
          days_info = tracks_downloaded_by_day
        )
        
        # Configurar arena automáticamente con los datos de ejemplo
        if (length(arena_files) > 0) {
          # Configuración predeterminada para los datos de ejemplo
          example_arena_config <- list(
            files = arena_files,
            arena_center = c(150, 150),  # Centro de la arena
            arena_radius = 60,           # Radio típico
            targets = list(
              "Arena_SW" = list(x = 150, y = 150, radius = 15),  # Target SW en (150, 150)
              "Arena_NE" = list(x = 90, y = 90, radius = 15)     # Target NE en posición estándar
            ),
            is_example_data = TRUE  # Flag para identificar que son datos de ejemplo
          )
          
          values$arena_config <- example_arena_config
          
          # También podemos actualizar la configuración del experimento si es necesario
          values$example_arena_defaults <- TRUE
        }
        
        example_loaded(TRUE)
        
        # Resumen de descarga
        total_days <- sum(sapply(tracks_downloaded_by_day, function(x) length(x) > 0))
        showNotification(
          paste("Datos de ejemplo cargados:", length(track_paths), "tracks de", total_days, "días,", 
                length(arena_files), "archivos de arena. Arena configurada automáticamente."),
          type = "message",
          duration = 8
        )
        
      }, error = function(e) {
        showNotification(
          paste("Error descargando datos de ejemplo:", e$message, 
                "\nPrueba la descarga manual desde GitHub."),
          type = "error",
          duration = 10
        )
      }, finally = {
        example_loading(FALSE)
      })
    })
    
    # Cargar archivo de experimento manual
    observeEvent(input$experiment_file, {
      req(input$experiment_file)
      
      tryCatch({
        # Leer archivo Excel
        exp_data <- readxl::read_excel(input$experiment_file$datapath)
        experiment_data(exp_data)
        values$experiment_data <- exp_data
        
        # Resetear flag de datos de ejemplo si estaba activo
        values$example_arena_defaults <- FALSE
        
        showNotification("Archivo de experimento cargado correctamente", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error leyendo archivo:", e$message), type = "error")
      })
    })
    
    # Cargar archivos de tracks manuales
    observeEvent(input$track_files, {
      req(input$track_files)
      
      tryCatch({
        # Crear directorio temporal para los archivos
        temp_dir <- tempfile(pattern = "track_files_")
        dir.create(temp_dir)
        
        # Copiar archivos al directorio temporal
        track_paths <- character(nrow(input$track_files))
        names(track_paths) <- input$track_files$name
        
        for (i in seq_len(nrow(input$track_files))) {
          original_path <- input$track_files$datapath[i]
          file_name <- input$track_files$name[i]
          new_path <- file.path(temp_dir, file_name)
          
          file.copy(original_path, new_path)
          track_paths[i] <- new_path
        }
        
        track_files_data(list(
          files = track_paths,
          directory = temp_dir,
          names = names(track_paths)
        ))
        
        values$track_files_data <- list(
          files = track_paths,
          directory = temp_dir,
          names = names(track_paths)
        )
        
        # Resetear flag de datos de ejemplo
        values$example_arena_defaults <- FALSE
        
        showNotification(
          paste("Cargados", length(track_paths), "archivos de tracks"),
          type = "message"
        )
        
      }, error = function(e) {
        showNotification(paste("Error cargando tracks:", e$message), type = "error")
      })
    })
    
    # Outputs de estado
    output$experiment_uploaded <- reactive({
      !is.null(experiment_data())
    })
    outputOptions(output, "experiment_uploaded", suspendWhenHidden = FALSE)
    
    output$tracks_uploaded <- reactive({
      !is.null(track_files_data())
    })
    outputOptions(output, "tracks_uploaded", suspendWhenHidden = FALSE)
    
    output$data_ready <- reactive({
      !is.null(experiment_data()) && !is.null(track_files_data())
    })
    outputOptions(output, "data_ready", suspendWhenHidden = FALSE)
    
    output$example_loading <- reactive({
      example_loading()
    })
    outputOptions(output, "example_loading", suspendWhenHidden = FALSE)
    
    output$example_loaded <- reactive({
      example_loaded()
    })
    outputOptions(output, "example_loaded", suspendWhenHidden = FALSE)
    
    # Mostrar datos
    output$experiment_preview <- DT::renderDataTable({
      req(experiment_data())
      
      DT::datatable(
        head(experiment_data(), 5),
        options = list(
          pageLength = 5,
          searching = FALSE,
          paging = FALSE,
          info = FALSE,
          scrollX = TRUE
        ),
        class = 'cell-border stripe'
      )
    })
    
    output$track_files_list <- renderText({
      req(track_files_data())
      
      # Si tenemos información de días, mostrarla organizadamente
      if (!is.null(track_files_data()$days_info)) {
        days_info <- track_files_data()$days_info
        result <- character()
        
        for (day_name in names(days_info)) {
          if (length(days_info[[day_name]]) > 0) {
            day_num <- gsub("day", "", day_name)
            result <- c(result, paste("Día", day_num, ":", length(days_info[[day_name]]), "tracks"))
          }
        }
        
        if (length(result) > 0) {
          paste(c(paste("Total:", length(track_files_data()$names), "archivos"), "", result), collapse = "\n")
        } else {
          paste(track_files_data()$names, collapse = "\n")
        }
      } else {
        paste(track_files_data()$names, collapse = "\n")
      }
    })
    
    output$tracks_status <- renderText({
      req(track_files_data())
      
      if (!is.null(track_files_data()$days_info)) {
        days_with_data <- sum(sapply(track_files_data()$days_info, function(x) length(x) > 0))
        paste("✅", length(track_files_data()$files), "archivos de tracks cargados de", days_with_data, "días")
      } else {
        paste("✅", length(track_files_data()$files), "archivos de tracks cargados correctamente")
      }
    })
    
    # Botón para ir a arena
    observeEvent(input$go_to_arena, {
      if (!is.null(parent_session)) {
        shinydashboard::updateTabItems(parent_session, "tabs", "arena_config")
      }
    })
  })
}