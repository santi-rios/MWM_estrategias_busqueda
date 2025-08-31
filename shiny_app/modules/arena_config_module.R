# Módulo para configuración de arena - Versión simplificada
# Visualiza dimensiones y genera texto para copiar/pegar

arenaConfigUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      conditionalPanel(
        condition = paste0("!output['", ns("experiment_loaded"), "']"),
        box(
          title = "⚠️ Configuración de Arena",
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          
          div(
            style = "text-align: center; padding: 40px;",
            icon("exclamation-triangle", style = "font-size: 48px; color: #f39c12;"),
            h4("Carga primero el experimento"),
            p("Para configurar las arenas, primero debes cargar el archivo de experimento en la pestaña 'Cargar Datos'.")
          )
        )
      )
    ),
    
    conditionalPanel(
      condition = paste0("output['", ns("experiment_loaded"), "']"),
      
      # Visualización de datos y dimensiones
      fluidRow(
          box(
            title = "🗺️ Visualización de Datos",
          status = "info",
          solidHeader = TRUE,
          width = 8,
          
          h4("Mapa de calor de las trayectorias"),
          p("Verifica que tus datos estén dentro de las dimensiones configuradas:"),
          
          plotOutput(ns("trajectory_heatmap"), height = "400px")
        ),
        
        box(
          title = "🔍 Información del Experimento",
          status = "info",
          solidHeader = TRUE,
          width = 4,
          
          h5("📋 Arenas detectadas:"),
          verbatimTextOutput(ns("arena_summary")),
          
          hr(),
          
          h5("📏 Estadísticas de coordenadas:"),
          verbatimTextOutput(ns("coordinate_stats"))
        )
      ),
      
      # Configuración de dimensiones
      fluidRow(
        box(
          title = "⚙️ Configuración de Arena",
          status = "primary",
          solidHeader = TRUE,
          width = 6,
          
          h4("Parámetros de la Arena"),
          p("Ajusta las dimensiones basándote en la visualización:"),
          
          fluidRow(
            column(6,
              h5("🎯 Centro de la Arena"),
              numericInput(ns("center_x"),
                          "Centro X:",
                          value = 133.655,
                          step = 0.001),
              
              numericInput(ns("center_y"),
                          "Centro Y:",
                          value = 103.5381,
                          step = 0.001)
            ),
            column(6,
              h5("📏 Dimensiones"),
              numericInput(ns("arena_radius"),
                          "Radio de la Arena:",
                          value = 95,
                          min = 1,
                          step = 0.1),
              
              numericInput(ns("goal_radius"),
                          "Radio del Objetivo:",
                          value = 10,
                          min = 1,
                          step = 0.1)
            )
          ),
          
          hr(),
          
          uiOutput(ns("goal_positions_ui")),
          
          br(),
          
          div(
            style = "text-align: center;",
            actionButton(ns("update_visualization"),
                        "🔄 Actualizar Visualización",
                        class = "btn-primary"),
            br(), br(),
            actionButton(ns("generate_config_text"),
                        "📝 Generar Configuración",
                        class = "btn-success btn-lg")
          )
        ),
        
        box(
            title = "📄 Configuración de Archivos",
          status = "success",
          solidHeader = TRUE,
          width = 6,
          
          conditionalPanel(
            condition = paste0("!output['", ns("config_generated"), "']"),
            div(
              style = "text-align: center; padding: 40px; color: #666;",
              icon("file-text", style = "font-size: 48px;"),
              h4("Configura primero las dimensiones"),
              p("Ajusta los parámetros y presiona 'Generar Configuración'")
            )
          ),
          
          conditionalPanel(
            condition = paste0("output['", ns("config_generated"), "']"),
            div(
              h4("📋 Archivos generados automáticamente"),
              p("La app creó los .txt con tus parámetros. Puedes revisar el contenido abajo o subir los tuyos para reemplazarlos."),
              
              uiOutput(ns("config_text_output")),
              
              hr(),
              
              h5("📤 Subir Archivos de Arena (opcional)"),
              p("Si tienes archivos propios, súbelos aquí para reemplazar los generados automáticamente:"),
              
              fileInput(ns("arena_files_upload"),
                       "Subir archivos de arena (.txt):",
                       multiple = TRUE,
                       accept = ".txt"),
              
              conditionalPanel(
                condition = paste0("output['", ns("arena_files_uploaded"), "']"),
                div(
                  style = "color: green; font-weight: bold; text-align: center; margin: 10px;",
                  "✅ Archivos de arena cargados correctamente"
                )
              )
            )
          )
        )
      )
    )
  )
}

arenaConfigServer <- function(id, values, parent_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Variables reactivas
    detected_arenas <- reactiveVal(NULL)
    config_generated <- reactiveVal(FALSE)
    arena_files_uploaded <- reactiveVal(FALSE)
  generated_dir <- reactiveVal(NULL)
  preview_points <- reactiveVal(NULL)
    
    # Verificar si el experimento está cargado
    output$experiment_loaded <- reactive({
      !is.null(values$experiment_data)
    })
    outputOptions(output, "experiment_loaded", suspendWhenHidden = FALSE)
    
    # Detectar arenas del experimento
    observe({
      req(values$experiment_data)
      
      # Obtener arenas únicas
      arena_col <- names(values$experiment_data)[grepl("^_Arena", names(values$experiment_data))][1]
      
      if (!is.null(arena_col)) {
        unique_arenas <- unique(values$experiment_data[[arena_col]])
        unique_arenas <- unique_arenas[!is.na(unique_arenas)]
        
        # Limpiar nombres de archivos (quitar .txt si existe)
        clean_arenas <- gsub("\\.txt$", "", unique_arenas)
        
        detected_arenas(clean_arenas)
      }
    })
    
    # Resumen de arenas
    output$arena_summary <- renderText({
      req(detected_arenas())
      arenas <- detected_arenas()
      paste0("Arenas detectadas: ", length(arenas), "\n",
             paste(arenas, collapse = ", "))
    })
    
    # Estadísticas de coordenadas (desde tracks cargados)
    output$coordinate_stats <- renderText({
      req(values$track_files_data)
      pts <- preview_points()
      if (is.null(pts) || nrow(pts) == 0) return("Sube tracks y pulsa 'Actualizar Visualización'")
      paste0(
        "Coordenadas X:\n",
        "  Min: ", round(min(pts$X, na.rm = TRUE), 2), "\n",
        "  Max: ", round(max(pts$X, na.rm = TRUE), 2), "\n",
        "  Centro aprox: ", round(mean(range(pts$X, na.rm = TRUE)), 2), "\n\n",
        "Coordenadas Y:\n",
        "  Min: ", round(min(pts$Y, na.rm = TRUE), 2), "\n",
        "  Max: ", round(max(pts$Y, na.rm = TRUE), 2), "\n",
        "  Centro aprox: ", round(mean(range(pts$Y, na.rm = TRUE)), 2)
      )
    })
    
    # Mapa de calor de trayectorias desde tracks
    output$trajectory_heatmap <- renderPlot({
      req(values$track_files_data)
      data_for_plot <- preview_points()
      if (!is.null(data_for_plot) && nrow(data_for_plot) > 0) {
        # Crear el mapa de calor
        p <- ggplot(data_for_plot, aes(x = X, y = Y)) +
          stat_density_2d_filled(alpha = 0.7, contour_var = "ndensity") +
          scale_fill_viridis_d(name = "Densidad") +
          theme_minimal() +
          theme(
            panel.background = element_rect(fill = "white"),
            plot.background = element_rect(fill = "white")
          ) +
          labs(
            title = "Mapa de Densidad de Trayectorias",
            subtitle = "Distribución de todas las coordenadas del experimento",
            x = "Coordenada X",
            y = "Coordenada Y"
          )
        
        # Agregar círculo de la arena si los parámetros están definidos
        if (!is.null(input$center_x) && !is.null(input$center_y) && !is.null(input$arena_radius)) {
          # Crear círculo de la arena
          circle_data <- data.frame(
            x = input$center_x + input$arena_radius * cos(seq(0, 2*pi, length.out = 100)),
            y = input$center_y + input$arena_radius * sin(seq(0, 2*pi, length.out = 100))
          )
          
          p <- p + 
            geom_path(data = circle_data, aes(x = x, y = y), 
                     color = "red", size = 1, inherit.aes = FALSE) +
            annotate("text", x = input$center_x, y = input$center_y + input$arena_radius + 10,
                    label = "Límite de Arena", color = "red", size = 3)
        }
        
        p
      } else {
        # Plot vacío si no hay datos
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "No hay datos para visualizar. Sube tracks y pulsa 'Actualizar Visualización'", size = 6) +
          theme_void()
      }
    })

    # Helper para leer X/Y desde archivos de track
    read_xy_from_file <- function(path, fmt = NULL) {
      if (is.null(fmt)) fmt <- "ethovision.3.csv"
      fmt <- tolower(trimws(fmt))
      tryCatch({
        if (grepl("ethovision", fmt)) {
          header_lines <- readLines(path, n = 200L, warn = FALSE)
          header_idx <- which(grepl("(^|,)Time(,|$).*X(,).*Y(,)", header_lines))[1]
          if (is.na(header_idx)) {
            header_idx <- which(grepl("Sample\\.? *no\\.,? *Time, *X, *Y", header_lines, ignore.case = TRUE))[1]
          }
          skip_n <- ifelse(is.na(header_idx), 0, header_idx - 1)
          df <- utils::read.csv(path, skip = skip_n, check.names = FALSE)
          nms <- tolower(gsub("[ .]", "_", names(df)))
          names(df) <- nms
          xcol <- which(nms == "x"); ycol <- which(nms == "y")
          if (length(xcol) == 1 && length(ycol) == 1) {
            out <- data.frame(X = suppressWarnings(as.numeric(df[[xcol]])),
                              Y = suppressWarnings(as.numeric(df[[ycol]])))
            out <- out[stats::complete.cases(out), , drop = FALSE]
            return(out)
          }
        } else if (fmt %in% c("raw.csv", "raw.csv2", "raw")) {
          sep <- ifelse(fmt == "raw.csv2", ";", ",")
          df <- utils::read.table(path, header = TRUE, sep = sep, check.names = FALSE)
          return(data.frame(X = as.numeric(df[["X"]]), Y = as.numeric(df[["Y"]])))
        } else if (fmt == "raw.tab") {
          df <- utils::read.table(path, header = TRUE, sep = "\t", check.names = FALSE)
          return(data.frame(X = as.numeric(df[["X"]]), Y = as.numeric(df[["Y"]])))
        } else {
          df <- utils::read.csv(path, check.names = FALSE)
          if (all(c("X", "Y") %in% names(df))) {
            return(data.frame(X = as.numeric(df$X), Y = as.numeric(df$Y)))
          }
        }
        return(data.frame())
      }, error = function(e) data.frame())
    }

    # Actualizar visualización: reunir puntos desde algunos tracks
    observeEvent(input$update_visualization, {
      req(values$track_files_data)
      fmt_map <- NULL
      if (!is.null(values$experiment_data) && "_TrackFile" %in% names(values$experiment_data)) {
        fmt_map <- values$experiment_data |>
          dplyr::select(`_TrackFile`, `_TrackFileFormat`) |>
          dplyr::distinct()
      }
      files <- values$track_files_data$files
      if (is.null(files) || length(files) == 0) return()
      sample_names <- utils::head(names(files), 5)
      pts_list <- lapply(sample_names, function(nm) {
        fmt <- NULL
        if (!is.null(fmt_map)) {
          hit <- fmt_map[fmt_map$`_TrackFile` == nm, "_TrackFileFormat", drop = TRUE]
          if (length(hit) > 0) fmt <- hit[[1]]
        }
        read_xy_from_file(files[[nm]], fmt)
      })
      # Concat y muestreo
      pts_list <- pts_list[vapply(pts_list, function(x) nrow(x) > 0, logical(1))]
      if (length(pts_list) == 0) {
        preview_points(data.frame())
      } else {
        pts <- do.call(rbind, pts_list)
        if (nrow(pts) > 50000) pts <- pts[sample.int(nrow(pts), 50000), , drop = FALSE]
        preview_points(pts)
      }
      showNotification("Visualización actualizada", type = "message")
    })
    
    # UI dinámico para posiciones de objetivos
    output$goal_positions_ui <- renderUI({
      req(detected_arenas())
      arenas <- detected_arenas()
      
      goal_inputs <- lapply(1:length(arenas), function(i) {
        arena_name <- arenas[i]
        
        div(
          style = "border: 1px solid #ddd; padding: 10px; margin: 5px 0; border-radius: 5px;",
          h5(paste("🎯 Arena:", arena_name), style = "font-weight: bold; color: #337ab7;"),
          fluidRow(
            column(6,
              numericInput(ns(paste0("goal_x_", i)),
                          "Objetivo X:",
                          value = ifelse(grepl("SW", arena_name), 121.8934, 140.42685),
                          step = 0.001)
            ),
            column(6,
              numericInput(ns(paste0("goal_y_", i)),
                          "Objetivo Y:",
                          value = ifelse(grepl("SW", arena_name), 154.6834, 51.32352),
                          step = 0.001)
            )
          )
        )
      })
      
      tagList(
        h4("🎯 Posiciones de Objetivos"),
        p("Configura las posiciones para cada arena detectada:"),
        do.call(tagList, goal_inputs)
      )
    })
    
    
    
    # Generar archivos de configuración y texto
    observeEvent(input$generate_config_text, {
      req(detected_arenas())
      
      tryCatch({
        # Directorio temporal para guardar archivos .txt
        dir_out <- tempfile("arena_files_")
        dir.create(dir_out)
        arenas <- detected_arenas()
        arena_files_info <- list()
        for (i in seq_along(arenas)) {
          arena_name <- arenas[i]
          goal_x <- input[[paste0("goal_x_", i)]]
          goal_y <- input[[paste0("goal_y_", i)]]
          if (is.null(goal_x) || is.null(goal_y)) {
            goal_x <- ifelse(grepl("SW", arena_name), 121.8934, 140.42685)
            goal_y <- ifelse(grepl("SW", arena_name), 154.6834, 51.32352)
          }
          old_goal_x <- ifelse(grepl("SW", arena_name), 140.42685, 121.8934)
          old_goal_y <- ifelse(grepl("SW", arena_name), 51.32352, 154.6834)
          arena_content <- paste(
            "type = mwm",
            "time.units = s",
            paste("arena.bounds = circle", input$center_x, input$center_y, input$arena_radius),
            paste("goal = circle", goal_x, goal_y, input$goal_radius),
            paste("old.goal = circle", old_goal_x, old_goal_y, input$goal_radius),
            sep = "\n"
          )
          file_name <- paste0(arena_name, ".txt")
          out_path <- file.path(dir_out, file_name)
          writeLines(arena_content, out_path)
          arena_files_info[[file_name]] <- out_path
        }
        # Guardar información globalmente
        values$arena_config <- list(
          files = arena_files_info,
          directory = dir_out,
          arenas_info = arenas
        )
        generated_dir(dir_out)
        config_generated(TRUE)
        arena_files_uploaded(TRUE)
  showNotification(paste0("✅ Generados ", length(arena_files_info), " archivos de arena"), type = "message")
      }, error = function(e) {
        showNotification(paste("Error generando configuración:", e$message), type = "error")
      })
    })
    
    # Output de texto de configuración
    output$config_text_output <- renderUI({
      req(config_generated(), detected_arenas())
      
  arenas <- detected_arenas()
      config_boxes <- lapply(1:length(arenas), function(i) {
        arena_name <- arenas[i]
        
        # Obtener coordenadas del objetivo para esta arena
        goal_x <- input[[paste0("goal_x_", i)]]
        goal_y <- input[[paste0("goal_y_", i)]]
        
        if (is.null(goal_x) || is.null(goal_y)) {
          # Valores por defecto si no se han especificado
          goal_x <- ifelse(grepl("SW", arena_name), 121.8934, 140.42685)
          goal_y <- ifelse(grepl("SW", arena_name), 154.6834, 51.32352)
        }
        
        # Para experimentos de reversión, necesitamos tanto goal como old.goal
        old_goal_x <- ifelse(grepl("SW", arena_name), 140.42685, 121.8934)
        old_goal_y <- ifelse(grepl("SW", arena_name), 51.32352, 154.6834)
        
        # Crear contenido del archivo
        arena_content <- paste(
          "type = mwm",
          "time.units = s",
          paste("arena.bounds = circle", input$center_x, input$center_y, input$arena_radius),
          paste("goal = circle", goal_x, goal_y, input$goal_radius),
          paste("old.goal = circle", old_goal_x, old_goal_y, input$goal_radius),
          sep = "\n"
        )
        
        div(
          style = "margin-bottom: 20px;",
          h5(paste0("📄 ", arena_name, ".txt"), style = "color: #337ab7;"),
          div(
            style = "background: #f8f9fa; border: 1px solid #ddd; padding: 10px; border-radius: 5px;",
            tags$pre(arena_content, style = "margin: 0; font-family: monospace; font-size: 12px;")
          ),
          br(),
          div(
            style = "text-align: right;",
            tags$button(
              "📋 Copiar",
              onclick = paste0("navigator.clipboard.writeText(`", gsub("`", "\\`", arena_content), "`); alert('Contenido copiado al portapapeles');"),
              class = "btn btn-sm btn-outline-primary"
            )
          )
        )
      })
      
      div(do.call(tagList, config_boxes))
    })
    
    # Indicador de configuración generada
    output$config_generated <- reactive({
      config_generated()
    })
    outputOptions(output, "config_generated", suspendWhenHidden = FALSE)
    
    # Manejar subida de archivos de arena
    observeEvent(input$arena_files_upload, {
      req(input$arena_files_upload)
      
      tryCatch({
        # Crear directorio temporal para archivos de arena
        arena_dir <- tempfile("arena_files_")
        dir.create(arena_dir)
        
        # Copiar archivos cargados
        arena_files_info <- list()
        
        for (i in 1:nrow(input$arena_files_upload)) {
          file_info <- input$arena_files_upload[i, ]
          dest_path <- file.path(arena_dir, file_info$name)
          file.copy(file_info$datapath, dest_path)
          
          arena_files_info[[file_info$name]] <- dest_path
        }
        
        # Guardar información en values para que esté disponible globalmente
        values$arena_config <- list(
          files = arena_files_info,
          directory = arena_dir,
          arenas_info = detected_arenas()
        )
        
        arena_files_uploaded(TRUE)
        
        showNotification(
          paste("✅ Cargados", length(arena_files_info), "archivos de arena correctamente"),
          type = "message"
        )
        
      }, error = function(e) {
        showNotification(paste("Error cargando archivos de arena:", e$message), type = "error")
      })
    })
    
    # Indicador de archivos subidos
    output$arena_files_uploaded <- reactive({
      arena_files_uploaded()
    })
    outputOptions(output, "arena_files_uploaded", suspendWhenHidden = FALSE)
  })
}
