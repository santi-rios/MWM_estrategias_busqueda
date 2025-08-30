# Script simplificado para análisis MWM sin interfaz gráfica
# Alternativa para usuarios que prefieren trabajar directamente en R

# Cargar librerías necesarias
suppressPackageStartupMessages({
  library(Rtrack)
  library(dplyr)
  library(ggplot2)
  library(viridis)
  library(readxl)
})

cat("🎯 MWM Analysis Script (Versión simplificada)\n")
cat("==============================================\n\n")

# CONFIGURACIÓN DEL USUARIO - MODIFICAR SEGÚN TUS DATOS
# ====================================================

# Rutas de archivos (modificar según tu estructura)
EXPERIMENT_FILE <- "data/example/Experiment_Example.csv"  # O .xlsx
DATA_DIR <- "data/example/"
OUTPUT_DIR <- "results/"

# Configuración de arena (modificar según tu setup)
ARENA_CONFIG <- list(
  type = "mwm",
  time_units = "s",
  center_x = 133.655,
  center_y = 103.5381,
  radius = 95,
  goal_x = 121.8934,
  goal_y = 154.6834,
  goal_radius = 10,
  goal_quadrant = "SW"
)

# Configuración de análisis
ANALYSIS_CONFIG <- list(
  threads = 0,  # 0 = automático
  confidence_threshold = 0.4,  # NULL para no aplicar umbral
  grouping_var = "Tratamiento",
  treatment_var = "Tratamiento",
  day_var = "_Day"
)

# Configuración de plots
PLOT_CONFIG <- list(
  color_palette = "viridis",
  resolution = 900,
  color_levels = 300,
  dpi = 300,
  width = 12,
  height = 8
)

# ====================================================
# INICIO DEL ANÁLISIS
# ====================================================

cat("📁 Verificando archivos...\n")

# Verificar que existen los archivos
if (!file.exists(EXPERIMENT_FILE)) {
  stop("❌ No se encuentra el archivo de experimento: ", EXPERIMENT_FILE)
}

if (!dir.exists(DATA_DIR)) {
  stop("❌ No se encuentra el directorio de datos: ", DATA_DIR)
}

# Crear directorio de salida
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
  cat("📁 Creado directorio de resultados:", OUTPUT_DIR, "\n")
}

# Función para crear archivo de arena
create_arena_file <- function(config, filename) {
  content <- paste(
    paste("type =", config$type),
    paste("time.units =", config$time_units),
    paste("arena.bounds = circle", config$center_x, config$center_y, config$radius),
    paste("goal = circle", config$goal_x, config$goal_y, config$goal_radius),
    sep = "\n"
  )
  
  writeLines(content, filename)
  cat("✅ Archivo de arena creado:", filename, "\n")
}

# Crear archivo de arena
arena_file <- file.path(OUTPUT_DIR, paste0("Arena_", ARENA_CONFIG$goal_quadrant, ".txt"))
create_arena_file(ARENA_CONFIG, arena_file)

cat("\n🔬 Procesando experimento con Rtrack...\n")

# Leer experimento
experiment <- tryCatch({
  Rtrack::read_experiment(
    EXPERIMENT_FILE,
    data.dir = DATA_DIR,
    arena.dir = dirname(arena_file),
    threads = ANALYSIS_CONFIG$threads
  )
}, error = function(e) {
  stop("❌ Error procesando experimento: ", e$message)
})

cat("✅ Experimento cargado:", length(experiment$metrics), "tracks procesados\n")

cat("\n🧠 Calculando estrategias de búsqueda...\n")

# Calcular estrategias
strategies <- Rtrack::call_strategy(experiment)

# Aplicar umbral si está configurado
if (!is.null(ANALYSIS_CONFIG$confidence_threshold)) {
  cat("🎯 Aplicando umbral de confianza:", ANALYSIS_CONFIG$confidence_threshold, "\n")
  strategies <- Rtrack::threshold_strategies(strategies, ANALYSIS_CONFIG$confidence_threshold)
  cat("✅ Tracks tras filtrado:", nrow(strategies$calls), "\n")
}

cat("\n📊 Generando análisis...\n")

# Función para clasificar estrategias
classify_strategies <- function(strategy_name) {
  allocentric <- c("directed search", "corrected path", "direct path")
  egocentric <- c("thigmotaxis", "circling", "random path", "scanning", "chaining")
  
  if (strategy_name %in% allocentric) return("Alocéntricas")
  if (strategy_name %in% egocentric) return("Egocéntricas")
  if (strategy_name == "perseverance") return("Perseverancia")
  return("Otras")
}

# Combinar datos
factors_df <- experiment$factors
strategies_df <- strategies$calls
strategies_df$Track_ID <- rownames(strategies_df)

# Merge datos
if ("_TrackID" %in% names(factors_df)) {
  merged_data <- merge(factors_df, strategies_df, 
                      by.x = "_TrackID", by.y = "Track_ID", all = TRUE)
} else {
  merged_data <- cbind(factors_df, strategies_df)
}

# Clasificar estrategias
merged_data$estrategias_hipo <- sapply(merged_data$name, classify_strategies)
merged_data$estrategias_hipo <- factor(merged_data$estrategias_hipo, 
                                      levels = c("Egocéntricas", "Alocéntricas", "Perseverancia"))

cat("\n🗺️ Generando mapas de densidad...\n")

# Configurar colores
color_func <- switch(PLOT_CONFIG$color_palette,
  "viridis" = viridis,
  "plasma" = plasma,
  "inferno" = inferno,
  "magma" = magma,
  plasma
)

# Crear mapas de densidad por grupo
if (ANALYSIS_CONFIG$grouping_var %in% names(merged_data)) {
  groups <- unique(merged_data[[ANALYSIS_CONFIG$grouping_var]])
  
  # Configurar layout para múltiples plots
  n_groups <- length(groups)
  if (n_groups > 1) {
    n_cols <- ceiling(sqrt(n_groups))
    n_rows <- ceiling(n_groups / n_cols)
    
    # Abrir dispositivo gráfico
    png(file.path(OUTPUT_DIR, "density_maps.png"), 
        width = PLOT_CONFIG$width * n_cols, 
        height = PLOT_CONFIG$height * n_rows, 
        units = "in", res = PLOT_CONFIG$dpi)
    
    par(mfrow = c(n_rows, n_cols))
    
    for (group in groups) {
      group_mask <- merged_data[[ANALYSIS_CONFIG$grouping_var]] == group
      group_indices <- which(group_mask)
      
      if (length(group_indices) > 0) {
        group_metrics <- experiment$metrics[group_indices]
        
        tryCatch({
          Rtrack::plot_density(
            group_metrics,
            title = paste(ANALYSIS_CONFIG$grouping_var, ":", group),
            col = color_func(PLOT_CONFIG$color_levels),
            resolution = PLOT_CONFIG$resolution,
            feature.col = "#E87FB0",
            feature.lwd = 4,
            legend = FALSE
          )
          cat("✅ Mapa creado para grupo:", group, "\n")
        }, error = function(e) {
          cat("⚠️ Error en mapa para grupo", group, ":", e$message, "\n")
        })
      }
    }
    
    dev.off()
    cat("💾 Mapas guardados en:", file.path(OUTPUT_DIR, "density_maps.png"), "\n")
  }
}

cat("\n📈 Generando análisis de estrategias...\n")

# Análisis de estrategias por grupo y día
if (ANALYSIS_CONFIG$day_var %in% names(merged_data)) {
  summary_data <- merged_data %>%
    filter(!is.na(estrategias_hipo)) %>%
    group_by(!!sym(ANALYSIS_CONFIG$grouping_var), !!sym(ANALYSIS_CONFIG$day_var), estrategias_hipo) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(!!sym(ANALYSIS_CONFIG$grouping_var), !!sym(ANALYSIS_CONFIG$day_var)) %>%
    mutate(
      total = sum(count),
      proportion = count / total
    )
  
  # Crear gráfico de estrategias
  p_strategies <- ggplot(summary_data, 
                        aes_string(x = ANALYSIS_CONFIG$day_var, 
                                  y = "proportion", 
                                  fill = "estrategias_hipo")) +
    geom_bar(stat = "identity", position = "fill", color = "black") +
    facet_wrap(as.formula(paste("~", ANALYSIS_CONFIG$grouping_var))) +
    scale_fill_manual(values = c("Egocéntricas" = "#EA5455", 
                                "Alocéntricas" = "#2D4059",
                                "Perseverancia" = "#F07B3F")) +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    theme(
      legend.position = "top",
      strip.background = element_rect(fill = "white", color = "gray"),
      strip.text = element_text(face = "bold")
    ) +
    labs(
      x = "Día",
      y = "Proporción de Estrategias",
      fill = "Tipo de Estrategia",
      title = "Distribución de Estrategias de Búsqueda"
    )
  
  # Guardar gráfico
  ggsave(file.path(OUTPUT_DIR, "strategy_analysis.png"), 
         p_strategies, 
         width = PLOT_CONFIG$width, 
         height = PLOT_CONFIG$height, 
         dpi = PLOT_CONFIG$dpi)
  
  cat("💾 Análisis de estrategias guardado en:", file.path(OUTPUT_DIR, "strategy_analysis.png"), "\n")
}

cat("\n💾 Guardando datos procesados...\n")

# Guardar datos principales
write.csv(merged_data, file.path(OUTPUT_DIR, "merged_data.csv"), row.names = FALSE)
write.csv(strategies$calls, file.path(OUTPUT_DIR, "strategies.csv"), row.names = TRUE)

if (exists("summary_data")) {
  write.csv(summary_data, file.path(OUTPUT_DIR, "strategy_summary.csv"), row.names = FALSE)
}

cat("\n📊 Resumen de resultados:\n")
cat("========================\n")
cat("- Tracks procesados:", length(experiment$metrics), "\n")
cat("- Sujetos únicos:", length(unique(merged_data$`_TargetID`)), "\n")
cat("- Días de experimento:", length(unique(merged_data[[ANALYSIS_CONFIG$day_var]])), "\n")
cat("- Confianza promedio:", round(mean(strategies$calls$confidence, na.rm = TRUE), 3), "\n")

# Tabla de estrategias por grupo
if (ANALYSIS_CONFIG$grouping_var %in% names(merged_data)) {
  cat("\n📋 Estrategias por grupo:\n")
  strategy_table <- table(merged_data[[ANALYSIS_CONFIG$grouping_var]], merged_data$estrategias_hipo)
  print(strategy_table)
}

cat("\n✅ ANÁLISIS COMPLETO\n")
cat("Todos los resultados están en:", OUTPUT_DIR, "\n")
cat("\nArchivos generados:\n")
cat("- density_maps.png: Mapas de densidad\n")
cat("- strategy_analysis.png: Análisis de estrategias\n")
cat("- merged_data.csv: Datos combinados\n")
cat("- strategies.csv: Estrategias llamadas\n")
cat("- strategy_summary.csv: Resumen por grupo\n")
cat("- Arena_", ARENA_CONFIG$goal_quadrant, ".txt: Configuración de arena\n\n")

cat("🎉 ¡Listo! Revisa los archivos en el directorio", OUTPUT_DIR, "\n")
