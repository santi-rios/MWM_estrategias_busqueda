# Módulo para configuración de arena
# Permite definir las dimensiones del pool y posiciones de objetivos

arenaConfigUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "🎯 Configuración de Arena",
        status = "primary",
        solidHeader = TRUE,
        width = 8,
        
        h4("Dimensiones del Pool"),
        p("Define las dimensiones de tu arena circular."),
        
        fluidRow(
          column(4,
            numericInput(ns("center_x"),
                        "Centro X:",
                        value = 133.655,
                        step = 0.001)
          ),
          column(4,
            numericInput(ns("center_y"),
                        "Centro Y:",
                        value = 103.5381,
                        step = 0.001)
          ),
          column(4,
            numericInput(ns("radius"),
                        "Radio:",
                        value = 95,
                        step = 0.1)
          )
        ),
        
        hr(),
        
        h4("Configuración de Objetivos"),
        p("Define las posiciones de los objetivos para diferentes fases."),
        
        tabsetPanel(
          tabPanel("Objetivo Principal",
            br(),
            fluidRow(
              column(3,
                numericInput(ns("goal_x"),
                            "Goal X:",
                            value = 121.8934,
                            step = 0.001)
              ),
              column(3,
                numericInput(ns("goal_y"),
                            "Goal Y:",
                            value = 154.6834,
                            step = 0.001)
              ),
              column(3,
                numericInput(ns("goal_radius"),
                            "Goal Radio:",
                            value = 10,
                            step = 0.1)
              ),
              column(3,
                selectInput(ns("goal_quadrant"),
                           "Cuadrante:",
                           choices = c("SW" = "SW", "SE" = "SE", 
                                     "NW" = "NW", "NE" = "NE"))
              )
            )
          ),
          
          tabPanel("Objetivo Reversa",
            br(),
            checkboxInput(ns("has_reversal"),
                         "¿Incluye pruebas de reversa?",
                         value = FALSE),
            
            conditionalPanel(
              condition = paste0("input['", ns("has_reversal"), "']"),
              fluidRow(
                column(3,
                  numericInput(ns("old_goal_x"),
                              "Old Goal X:",
                              value = 145,
                              step = 0.001)
                ),
                column(3,
                  numericInput(ns("old_goal_y"),
                              "Old Goal Y:",
                              value = 53,
                              step = 0.001)
                ),
                column(3,
                  numericInput(ns("old_goal_radius"),
                              "Old Goal Radio:",
                              value = 10,
                              step = 0.1)
                ),
                column(3,
                  selectInput(ns("old_goal_quadrant"),
                             "Cuadrante Anterior:",
                             choices = c("SW" = "SW", "SE" = "SE", 
                                       "NW" = "NW", "NE" = "NE"),
                             selected = "NE")
                )
              )
            )
          )
        ),
        
        hr(),
        
        h4("Configuración de Tiempo"),
        fluidRow(
          column(6,
            selectInput(ns("time_units"),
                       "Unidades de Tiempo:",
                       choices = c("Segundos (s)" = "s",
                                 "Milisegundos (ms)" = "ms",
                                 "Minutos (m)" = "m"),
                       selected = "s")
          ),
          column(6,
            numericInput(ns("time_conversion"),
                        "Factor de Conversión:",
                        value = 1,
                        step = 0.0001,
                        min = 0)
          )
        ),
        
        br(),
        
        div(
          style = "text-align: center;",
          actionButton(ns("generate_arena"),
                      "🏗️ Generar Configuración de Arena",
                      class = "btn-success btn-lg")
        )
      ),
      
      box(
        title = "👁️ Vista Previa",
        status = "info",
        solidHeader = TRUE,
        width = 4,
        
        plotOutput(ns("arena_preview"), height = "400px"),
        
        br(),
        
        conditionalPanel(
          condition = paste0("output['", ns("arena_generated"), "']"),
          div(
            style = "color: green; text-align: center; font-weight: bold;",
            "✅ Arena configurada correctamente"
          ),
          br(),
          div(
            style = "text-align: center;",
            actionButton(ns("proceed_to_analysis"),
                        "⚙️ Procesar Análisis",
                        class = "btn-warning btn-lg")
          )
        )
      )
    ),
    
    fluidRow(
      column(12,
        conditionalPanel(
          condition = paste0("output['", ns("arena_generated"), "']"),
          box(
            title = "📄 Archivos de Arena Generados",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            
            h5("Se han generado los siguientes archivos de configuración:"),
            
            verbatimTextOutput(ns("arena_files_info"))
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
    arena_config <- reactiveVal(NULL)
    arena_files <- reactiveVal(NULL)
    
    # Vista previa de la arena
    output$arena_preview <- renderPlot({
      # Crear un plot básico de la arena con las coordenadas actuales
      center_x <- ifelse(is.na(input$center_x), 133.655, input$center_x)
      center_y <- ifelse(is.na(input$center_y), 103.5381, input$center_y)
      radius <- ifelse(is.na(input$radius), 95, input$radius)
      
      goal_x <- ifelse(is.na(input$goal_x), 121.8934, input$goal_x)
      goal_y <- ifelse(is.na(input$goal_y), 154.6834, input$goal_y)
      goal_radius <- ifelse(is.na(input$goal_radius), 10, input$goal_radius)
      
      # Crear el plot
      theta <- seq(0, 2*pi, length.out = 100)
      
      # Círculo del pool
      pool_x <- center_x + radius * cos(theta)
      pool_y <- center_y + radius * sin(theta)
      
      # Círculo del objetivo
      goal_circle_x <- goal_x + goal_radius * cos(theta)
      goal_circle_y <- goal_y + goal_radius * sin(theta)
      
      plot(pool_x, pool_y, type = "l", lwd = 3, col = "blue",
           asp = 1, main = "Vista Previa de Arena",
           xlab = "X", ylab = "Y")
      
      # Agregar objetivo principal
      polygon(goal_circle_x, goal_circle_y, col = "red", border = "darkred", lwd = 2)
      points(goal_x, goal_y, pch = 19, col = "darkred", cex = 1.5)
      text(goal_x, goal_y - goal_radius - 5, "Goal", col = "darkred", font = 2)
      
      # Agregar objetivo anterior si existe
      if (!is.null(input$has_reversal) && input$has_reversal) {
        old_goal_x <- ifelse(is.na(input$old_goal_x), 145, input$old_goal_x)
        old_goal_y <- ifelse(is.na(input$old_goal_y), 53, input$old_goal_y)
        old_goal_radius <- ifelse(is.na(input$old_goal_radius), 10, input$old_goal_radius)
        
        old_goal_circle_x <- old_goal_x + old_goal_radius * cos(theta)
        old_goal_circle_y <- old_goal_y + old_goal_radius * sin(theta)
        
        polygon(old_goal_circle_x, old_goal_circle_y, 
                col = alpha("orange", 0.7), border = "darkorange", lwd = 2, lty = 2)
        points(old_goal_x, old_goal_y, pch = 19, col = "darkorange", cex = 1.5)
        text(old_goal_x, old_goal_y - old_goal_radius - 5, "Old Goal", col = "darkorange", font = 2)
      }
      
      # Agregar centro
      points(center_x, center_y, pch = 3, col = "black", cex = 2, lwd = 3)
      text(center_x, center_y + 15, "Centro", col = "black", font = 2)
      
      # Agregar cuadrantes
      abline(h = center_y, col = "gray", lty = 3)
      abline(v = center_x, col = "gray", lty = 3)
      
      # Labels de cuadrantes
      text(center_x + radius*0.7, center_y + radius*0.7, "NE", col = "gray", font = 2)
      text(center_x - radius*0.7, center_y + radius*0.7, "NW", col = "gray", font = 2)
      text(center_x - radius*0.7, center_y - radius*0.7, "SW", col = "gray", font = 2)
      text(center_x + radius*0.7, center_y - radius*0.7, "SE", col = "gray", font = 2)
    })
    
    # Generar configuración de arena
    observeEvent(input$generate_arena, {
      tryCatch({
        # Crear directorio temporal para archivos de arena
        arena_dir <- file.path(tempdir(), "arenas")
        dir.create(arena_dir, showWarnings = FALSE)
        
        # Configuración básica
        config <- list(
          type = "mwm",
          time_units = input$time_units,
          arena_bounds = list(
            shape = "circle",
            center_x = input$center_x,
            center_y = input$center_y,
            radius = input$radius
          ),
          goal = list(
            shape = "circle",
            center_x = input$goal_x,
            center_y = input$goal_y,
            radius = input$goal_radius,
            quadrant = input$goal_quadrant
          )
        )
        
        # Archivo de arena principal
        arena_content <- paste(
          paste("type =", config$type),
          paste("time.units =", config$time_units),
          paste("arena.bounds = circle", config$arena_bounds$center_x, 
                config$arena_bounds$center_y, config$arena_bounds$radius),
          paste("goal = circle", config$goal$center_x, 
                config$goal$center_y, config$goal$radius),
          sep = "\n"
        )
        
        arena_file <- file.path(arena_dir, paste0("Arena_", config$goal$quadrant, ".txt"))
        writeLines(arena_content, arena_file)
        
        files_created <- c(arena_file)
        
        # Archivo de arena reversa si es necesario
        if (!is.null(input$has_reversal) && input$has_reversal) {
          config$old_goal <- list(
            shape = "circle",
            center_x = input$old_goal_x,
            center_y = input$old_goal_y,
            radius = input$old_goal_radius,
            quadrant = input$old_goal_quadrant
          )
          
          arena_rev_content <- paste(
            paste("type =", config$type),
            paste("time.units =", config$time_units),
            paste("arena.bounds = circle", config$arena_bounds$center_x, 
                  config$arena_bounds$center_y, config$arena_bounds$radius),
            paste("goal = circle", config$goal$center_x, 
                  config$goal$center_y, config$goal$radius),
            paste("old.goal = circle", config$old_goal$center_x, 
                  config$old_goal$center_y, config$old_goal$radius),
            sep = "\n"
          )
          
          arena_rev_file <- file.path(arena_dir, paste0("Arena_", config$goal$quadrant, "_Rev.txt"))
          writeLines(arena_rev_content, arena_rev_file)
          files_created <- c(files_created, arena_rev_file)
        }
        
        # Guardar configuración
        arena_config(config)
        arena_files(list(
          directory = arena_dir,
          files = files_created,
          config = config
        ))
        
        # Pasar a values para otros módulos
        values$arena_config <- config
        values$arena_files <- arena_files()
        
        showNotification("Configuración de arena generada correctamente", type = "success")
        
      }, error = function(e) {
        showNotification(paste("Error al generar arena:", e$message), type = "error")
      })
    })
    
    # Output indicators
    output$arena_generated <- reactive({
      !is.null(arena_config())
    })
    outputOptions(output, "arena_generated", suspendWhenHidden = FALSE)
    
    # Información de archivos generados
    output$arena_files_info <- renderText({
      if (!is.null(arena_files())) {
        files_info <- arena_files()
        paste(
          "📁 Directorio:", files_info$directory,
          "\n📄 Archivos creados:",
          paste("  -", basename(files_info$files), collapse = "\n"),
          "\n\n🔧 Configuración:",
          paste("  - Tipo:", files_info$config$type),
          paste("  - Unidades de tiempo:", files_info$config$time_units),
          paste("  - Centro:", files_info$config$arena_bounds$center_x, ",", files_info$config$arena_bounds$center_y),
          paste("  - Radio:", files_info$config$arena_bounds$radius),
          paste("  - Goal:", files_info$config$goal$center_x, ",", files_info$config$goal$center_y, 
                "(radio:", files_info$config$goal$radius, ")"),
          sep = "\n"
        )
      }
    })
    
    # Botón para proceder
    observeEvent(input$proceed_to_analysis, {
      if (!is.null(parent_session)) {
        shinydashboard::updateTabItems(parent_session, "tabs", "analysis")
      } else {
        shinydashboard::updateTabItems(session$parent, "tabs", "analysis")
      }
    })
  })
}
