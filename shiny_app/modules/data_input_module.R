# Módulo para carga de datos
# Permite al usuario cargar el archivo de experimento y los tracks

dataInputUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(6,
        wellPanel(
          h4("📊 Archivo de Experimento"),
          p("Sube tu archivo Excel (.xlsx) con la descripción del experimento."),
          p("Debe contener las columnas: _TrackID, _TargetID, _Day, _Trial, _Arena, _TrackFile, _TrackFileFormat"),
          
          fileInput(ns("experiment_file"),
                   "Seleccionar archivo de experimento:",
                   accept = c(".xlsx", ".xls"),
                   placeholder = "Ningún archivo seleccionado"),
          
          conditionalPanel(
            condition = paste0("output['", ns("experiment_uploaded"), "']"),
            div(
              style = "color: green; font-weight: bold;",
              "✅ Archivo cargado correctamente"
            )
          )
        )
      ),
      
      column(6,
        wellPanel(
          h4("📁 Archivos de Tracks"),
          p("Sube todos los archivos de tracks mencionados en tu experimento."),
          p("Formatos soportados: .csv, .txt"),
          
          fileInput(ns("track_files"),
                   "Seleccionar archivos de tracks:",
                   multiple = TRUE,
                   accept = c(".csv", ".txt"),
                   placeholder = "Ningún archivo seleccionado"),
          
          conditionalPanel(
            condition = paste0("output['", ns("tracks_uploaded"), "']"),
            div(
              style = "color: green; font-weight: bold;",
              textOutput(ns("tracks_status"))
            )
          )
        )
      )
    ),
    
    fluidRow(
      column(12,
        conditionalPanel(
          condition = paste0("output['", ns("experiment_uploaded"), "'] && output['", ns("tracks_uploaded"), "']"),
          wellPanel(
            h4("👀 Vista Previa de Datos"),
            
            tabsetPanel(
              tabPanel("Experimento", 
                       br(),
                       DT::dataTableOutput(ns("experiment_preview"))
              ),
              tabPanel("Archivos Detectados",
                       br(),
                       DT::dataTableOutput(ns("files_summary"))
              ),
              tabPanel("Ejemplo de Track",
                       br(),
                       DT::dataTableOutput(ns("track_preview"))
              )
            )
          )
        )
      )
    ),
    
    fluidRow(
      column(12,
        conditionalPanel(
          condition = paste0("output['", ns("data_ready"), "']"),
          div(
            style = "text-align: center; margin: 20px;",
            actionButton(ns("proceed_to_arena"),
                        "🎯 Configurar Arena",
                        class = "btn-primary btn-lg",
                        icon = icon("arrow-right"))
          )
        )
      )
    )
  )
}

dataInputServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Variables reactivas locales
    experiment_data <- reactiveVal(NULL)
    track_files_data <- reactiveVal(NULL)
    
    # Cargar archivo de experimento
    observeEvent(input$experiment_file, {
      req(input$experiment_file)
      
      tryCatch({
        # Leer el archivo Excel
        exp_data <- readxl::read_excel(input$experiment_file$datapath)
        
        # Validar columnas requeridas
        required_cols <- c("_TrackID", "_TargetID", "_Day", "_Trial", 
                          "_Arena", "_TrackFile", "_TrackFileFormat")
        missing_cols <- setdiff(required_cols, names(exp_data))
        
        if (length(missing_cols) > 0) {
          showNotification(
            paste("Faltan columnas requeridas:", paste(missing_cols, collapse = ", ")),
            type = "error"
          )
          return()
        }
        
        experiment_data(exp_data)
        values$experiment_data <- exp_data
        
        showNotification("Archivo de experimento cargado correctamente", type = "success")
        
      }, error = function(e) {
        showNotification(paste("Error al cargar archivo:", e$message), type = "error")
      })
    })
    
    # Cargar archivos de tracks
    observeEvent(input$track_files, {
      req(input$track_files)
      
      tryCatch({
        # Crear un directorio temporal para los tracks
        track_dir <- tempfile()
        dir.create(track_dir)
        
        # Copiar archivos cargados al directorio temporal
        track_files_info <- list()
        
        for (i in 1:nrow(input$track_files)) {
          file_info <- input$track_files[i, ]
          dest_path <- file.path(track_dir, file_info$name)
          file.copy(file_info$datapath, dest_path)
          
          track_files_info[[file_info$name]] <- dest_path
        }
        
        track_files_data(list(
          files = track_files_info,
          directory = track_dir
        ))
        
        showNotification(
          paste("Cargados", length(track_files_info), "archivos de tracks"),
          type = "success"
        )
        
      }, error = function(e) {
        showNotification(paste("Error al cargar tracks:", e$message), type = "error")
      })
    })
    
    # Output indicators
    output$experiment_uploaded <- reactive({
      !is.null(experiment_data())
    })
    outputOptions(output, "experiment_uploaded", suspendWhenHidden = FALSE)
    
    output$tracks_uploaded <- reactive({
      !is.null(track_files_data())
    })
    outputOptions(output, "tracks_uploaded", suspendWhenHidden = FALSE)
    
    output$tracks_status <- renderText({
      if (!is.null(track_files_data())) {
        paste("✅", length(track_files_data()$files), "archivos cargados")
      }
    })
    
    output$data_ready <- reactive({
      !is.null(experiment_data()) && !is.null(track_files_data())
    })
    outputOptions(output, "data_ready", suspendWhenHidden = FALSE)
    
    # Previews
    output$experiment_preview <- DT::renderDataTable({
      req(experiment_data())
      DT::datatable(
        experiment_data(),
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = 'ltip'
        ),
        class = 'cell-border stripe hover'
      )
    })
    
    output$files_summary <- DT::renderDataTable({
      req(experiment_data(), track_files_data())
      
      exp_files <- unique(experiment_data()$`_TrackFile`)
      uploaded_files <- names(track_files_data()$files)
      
      files_df <- data.frame(
        `Archivo Requerido` = exp_files,
        `Estado` = ifelse(exp_files %in% uploaded_files, "✅ Encontrado", "❌ Faltante"),
        `Archivo Cargado` = ifelse(exp_files %in% uploaded_files, exp_files, ""),
        stringsAsFactors = FALSE
      )
      
      DT::datatable(
        files_df,
        options = list(
          pageLength = 15,
          dom = 'ltip'
        ),
        class = 'cell-border stripe hover'
      ) %>%
        DT::formatStyle(
          "Estado",
          backgroundColor = DT::styleEqual("✅ Encontrado", "#d4edda")
        )
    })
    
    output$track_preview <- DT::renderDataTable({
      req(track_files_data())
      
      # Mostrar preview del primer archivo
      first_file <- track_files_data()$files[[1]]
      
      tryCatch({
        # Intentar leer como CSV
        preview_data <- read.csv(first_file, nrows = 10)
        
        DT::datatable(
          preview_data,
          options = list(
            scrollX = TRUE,
            pageLength = 10,
            dom = 'ltip'
          ),
          class = 'cell-border stripe hover'
        )
        
      }, error = function(e) {
        # Si falla, mostrar mensaje
        data.frame(Error = "No se pudo previsualizar el archivo")
      })
    })
    
    # Almacenar datos en values para otros módulos
    observe({
      values$track_files_data <- track_files_data()
    })
    
    # Botón para proceder
    observeEvent(input$proceed_to_arena, {
      updateTabItems(session = session$parent, "tabs", "arena_config")
    })
  })
}
