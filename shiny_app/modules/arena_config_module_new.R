# Módulo para configuración de arena - Versión mejorada
# Permite detectar múltiples arenas y generar archivos para descarga

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
      
      # Detección de arenas
      fluidRow(
        box(
          title = "🔍 Detección de Arenas",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          
          h4("Arenas detectadas en tu experimento:"),
          
          conditionalPanel(
            condition = paste0("output['", ns("arenas_detected"), "']"),
            div(
              style = "background: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0;",
              h5("📋 Resumen:"),
              textOutput(ns("arena_summary")),
              br(),
              DT::dataTableOutput(ns("arena_details_table"))
            )
          )
        )
      ),
      
      # Generación de archivos de arena
      fluidRow(
        box(
          title = "⚙️ Configuración de Arena",
          status = "primary",
          solidHeader = TRUE,
          width = 6,
          
          h4("Parámetros de la Arena"),
          p("Configura las dimensiones de tu arena circular:"),
          
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
          
          h4("🎯 Posiciones de Objetivos"),
          p("Configura las posiciones de los objetivos para cada arena:"),
          
          conditionalPanel(
            condition = paste0("output['", ns("arenas_detected"), "']"),
            uiOutput(ns("goal_positions_ui"))
          ),
          
          br(),
          
          div(
            style = "text-align: center;",
            actionButton(ns("generate_arena_files"),
                        "📝 Generar Archivos de Arena",
                        class = "btn-success btn-lg")
          )
        ),
        
        box(
          title = "📁 Descarga de Archivos",
          status = "success",
          solidHeader = TRUE,
          width = 6,
          
          conditionalPanel(
            condition = paste0("!output['", ns("files_generated"), "']"),
            div(
              style = "text-align: center; padding: 40px; color: #666;",
              icon("download", style = "font-size: 48px;"),
              h4("Generar archivos primero"),
              p("Configura los parámetros y presiona 'Generar Archivos de Arena'")
            )
          ),
          
          conditionalPanel(
            condition = paste0("output['", ns("files_generated"), "']"),
            div(
              h4("✅ Archivos generados correctamente"),
              p("Descarga los archivos de arena y súbelos en la siguiente sección:"),
              br(),
              
              div(id = ns("download_buttons")),
              
              hr(),
              
              h5("📤 Subir Archivos de Arena"),
              p("Después de descargar, sube aquí los archivos de arena:"),
              
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
    arena_files_content <- reactiveVal(NULL)
    files_generated <- reactiveVal(FALSE)
    arena_files_uploaded <- reactiveVal(FALSE)
    
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
        
        # Crear tabla de detalles
        arena_details <- data.frame(
          Arena = clean_arenas,
          Trials = sapply(unique_arenas, function(x) {
            sum(values$experiment_data[[arena_col]] == x, na.rm = TRUE)
          }),
          stringsAsFactors = FALSE
        )
        
        detected_arenas(arena_details)
      }
    })
    
    # Indicador de arenas detectadas
    output$arenas_detected <- reactive({
      !is.null(detected_arenas())
    })
    outputOptions(output, "arenas_detected", suspendWhenHidden = FALSE)
    
    # Resumen de arenas
    output$arena_summary <- renderText({
      req(detected_arenas())
      arenas <- detected_arenas()
      paste0("Se detectaron ", nrow(arenas), " arena(s) diferentes: ", 
             paste(arenas$Arena, collapse = ", "))
    })
    
    # Tabla de detalles de arenas
    output$arena_details_table <- DT::renderDataTable({
      req(detected_arenas())
      
      DT::datatable(
        detected_arenas(),
        options = list(
          pageLength = 10,
          searching = FALSE,
          paging = FALSE,
          info = FALSE,
          dom = 't'
        ),
        class = 'cell-border stripe hover'
      )
    })
    
    # UI dinámico para posiciones de objetivos
    output$goal_positions_ui <- renderUI({
      req(detected_arenas())
      arenas <- detected_arenas()
      
      goal_inputs <- lapply(1:nrow(arenas), function(i) {
        arena_name <- arenas$Arena[i]
        
        div(
          style = "border: 1px solid #ddd; padding: 10px; margin: 5px 0; border-radius: 5px;",
          h6(paste("Arena:", arena_name), style = "font-weight: bold;"),
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
      
      do.call(tagList, goal_inputs)
    })
    
    # Generar archivos de arena
    observeEvent(input$generate_arena_files, {
      req(detected_arenas())
      
      tryCatch({
        arenas <- detected_arenas()
        files_content <- list()
        
        for (i in 1:nrow(arenas)) {
          arena_name <- arenas$Arena[i]
          
          # Obtener coordenadas del objetivo para esta arena
          goal_x <- input[[paste0("goal_x_", i)]]
          goal_y <- input[[paste0("goal_y_", i)]]
          
          if (is.null(goal_x) || is.null(goal_y)) {
            # Valores por defecto si no se han especificado
            goal_x <- ifelse(grepl("SW", arena_name), 121.8934, 140.42685)
            goal_y <- ifelse(grepl("SW", arena_name), 154.6834, 51.32352)
          }
          
          # Crear contenido del archivo
          arena_content <- paste(
            "type = mwm",
            "time.units = s",
            "trial.length = 120",
            "",
            "# For circular pool/goal, give values in this order:",
            "# shape centre.x centre.y radius",
            "",
            paste("arena.bounds = circle", input$center_x, input$center_y, input$arena_radius),
            "",
            paste("goal = circle", goal_x, goal_y, input$goal_radius),
            "",
            sep = "\n"
          )
          
          files_content[[arena_name]] <- arena_content
        }
        
        arena_files_content(files_content)
        files_generated(TRUE)
        
        # Generar botones de descarga dinámicamente
        output$download_buttons <- renderUI({
          req(arena_files_content())
          
          download_buttons <- lapply(names(arena_files_content()), function(arena_name) {
            downloadButton(
              ns(paste0("download_", gsub("[^A-Za-z0-9]", "_", arena_name))),
              paste("📁", arena_name, ".txt"),
              class = "btn btn-primary",
              style = "margin: 5px;"
            )
          })
          
          div(do.call(tagList, download_buttons))
        })
        
        # Crear handlers de descarga dinámicamente
        lapply(names(arena_files_content()), function(arena_name) {
          button_id <- paste0("download_", gsub("[^A-Za-z0-9]", "_", arena_name))
          
          output[[button_id]] <- downloadHandler(
            filename = function() {
              paste0(arena_name, ".txt")
            },
            content = function(file) {
              writeLines(arena_files_content()[[arena_name]], file)
            }
          )
        })
        
        showNotification("Archivos de arena generados correctamente. Descárgalos y súbelos abajo.", 
                        type = "success", duration = 8)
        
      }, error = function(e) {
        showNotification(paste("Error generando archivos:", e$message), type = "error")
      })
    })
    
    # Indicador de archivos generados
    output$files_generated <- reactive({
      files_generated()
    })
    outputOptions(output, "files_generated", suspendWhenHidden = FALSE)
    
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
          paste("Cargados", length(arena_files_info), "archivos de arena correctamente"),
          type = "success"
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
