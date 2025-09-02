# Funciones auxiliares para procesamiento de datos MWM
# Contiene las funciones principales para procesar experimentos con Rtrack

# Función auxiliar para crear archivo de arena
create_arena_file <- function(arena_info, arena_name, output_dir) {
  
  # Extraer configuración de arena (estructura anidada del módulo)
  center_x <- arena_info$arena_bounds$center_x
  center_y <- arena_info$arena_bounds$center_y
  radius <- arena_info$arena_bounds$radius
  
  # Extraer configuración de objetivo
  goal_x <- arena_info$goal$center_x
  goal_y <- arena_info$goal$center_y
  goal_radius <- arena_info$goal$radius
  
  # Crear contenido del archivo de arena siguiendo el formato exacto de Rtrack
  arena_content <- paste(
    "type = mwm",
    "",
    "time.units = s",
    "",
    paste("arena.bounds = circle", center_x, center_y, radius),
    "",
    paste("goal = circle", goal_x, goal_y, goal_radius),
    "",
    sep = "\n"
  )
  
  # Crear archivo de arena
  arena_file <- file.path(output_dir, paste0(arena_name, ".txt"))
  writeLines(arena_content, arena_file)
  
  return(arena_file)
}

# Función principal para procesar experimento MWM
process_mwm_experiment <- function(experiment_file, data_dir, project_dir = NULL, threads = 0) {
  
  # Validaciones de entrada
  if (is.null(experiment_file) || !file.exists(experiment_file)) {
    stop("El archivo de experimento no existe: ", experiment_file)
  }
  
  if (is.null(data_dir) || !dir.exists(data_dir)) {
    stop("El directorio de datos no existe: ", data_dir)
  }
  
  # Si no se especifica project_dir, usar el directorio del archivo de experimento
  if (is.null(project_dir)) {
    project_dir <- dirname(experiment_file)
  }
  
  if (!dir.exists(project_dir)) {
    stop("El directorio del proyecto no existe: ", project_dir)
  }
  
  # Validar threads
  if (is.null(threads) || length(threads) == 0 || !is.numeric(threads)) {
    threads <- 0  # Usar valor por defecto
  }
  
  # Leer y validar datos del experimento antes de procesar
  tryCatch({
    cat("Leyendo archivo de experimento...\n")
    exp_data <- readxl::read_excel(experiment_file)
    
    # Validar estructura del experimento
    validate_experiment_structure(exp_data)
    
    # Verificar que los archivos de tracks existen
    track_files <- unique(exp_data$`_TrackFile`)
    cat("Archivos de tracks esperados:", length(track_files), "\n")
    
    missing_files <- c()
    available_files <- c()
    
    for (track_file in track_files) {
      track_path <- file.path(data_dir, track_file)
      if (!file.exists(track_path)) {
        missing_files <- c(missing_files, track_file)
      } else {
        # Verificar que el archivo no esté vacío
        file_size <- file.info(track_path)$size
        if (file_size == 0) {
          missing_files <- c(missing_files, paste(track_file, "(vacío)"))
        } else {
          available_files <- c(available_files, track_file)
        }
      }
    }
    
    # Si faltan archivos, filtrar el experimento para usar solo los disponibles
    if (length(missing_files) > 0) {
      cat("Archivos faltantes:", paste(missing_files, collapse = ", "), "\n")
      cat("Continuando con", length(available_files), "archivos disponibles\n")
      
      # Filtrar el experimento para incluir solo tracks disponibles
      exp_data <- exp_data[exp_data$`_TrackFile` %in% available_files, ]
      
      if (nrow(exp_data) == 0) {
        stop("Ninguno de los archivos de tracks especificados está disponible")
      }
      
      # Guardar el experimento filtrado
      writexl::write_xlsx(exp_data, experiment_file)
      cat("Experimento filtrado guardado con", nrow(exp_data), "tracks\n")
    }
    
    cat("Todos los archivos de tracks están presentes.\n")
    
  }, error = function(e) {
    stop(paste("Error en validación previa:", e$message))
  })
  
  # Leer experimento con Rtrack
  tryCatch({
    cat("Procesando experimento con parámetros:\n")
    cat("  Archivo:", experiment_file, "\n")
    cat("  Directorio de datos:", data_dir, "\n") 
    cat("  Directorio del proyecto:", project_dir, "\n")
    cat("  Hilos:", threads, "\n")
    
    experiment <- Rtrack::read_experiment(
      filename = experiment_file,
      data.dir = data_dir,
      project.dir = project_dir,
      threads = threads,
      verbose = TRUE
    )
    
    # Validar que el experimento se procesó correctamente
    if (is.null(experiment) || length(experiment$metrics) == 0) {
      stop("El experimento no se procesó correctamente - no se encontraron métricas")
    }
    
    cat("Experimento procesado exitosamente con", length(experiment$metrics), "tracks.\n")
    
    return(experiment)
    
  }, error = function(e) {
    stop(paste("Error procesando experimento:", e$message))
  })
}

# Función para procesar datos de estrategias para análisis estadístico
process_strategy_data <- function(experiment, strategies, grouping_vars = NULL, 
                                 treatment_var = NULL, day_var = "_Day") {
  
  # Exportar resultados completos usando Rtrack
  tryCatch({
    results_df <- Rtrack::export_results(experiment)
    
    # Combinar con estrategias
    strategies_df <- strategies$calls
    
    # Agregar Track ID si no existe en strategies
    if (!"Track_ID" %in% names(strategies_df)) {
      strategies_df$Track_ID <- rownames(strategies_df)
    }
    
    # Merge con resultados exportados
    if ("Track_ID" %in% names(results_df) && "Track_ID" %in% names(strategies_df)) {
      merged_data <- merge(results_df, strategies_df, 
                          by = "Track_ID", all.x = TRUE)
    } else {
      # Fallback: usar índices si no hay Track_ID
      merged_data <- cbind(results_df, strategies_df)
    }
    
    # Clasificar estrategias
    merged_data <- classify_strategies(merged_data)
    
    # Procesar para análisis longitudinal si se especifica
    if (!is.null(day_var) && day_var %in% names(merged_data)) {
      longitudinal_data <- create_longitudinal_data(merged_data, grouping_vars, treatment_var, day_var)
      
      return(list(
        merged_data = merged_data,
        longitudinal_data = longitudinal_data,
        results_export = results_df,
        strategies_data = strategies_df,
        grouping_vars = grouping_vars,
        treatment_var = treatment_var,
        day_var = day_var
      ))
    }
    
    return(list(
      merged_data = merged_data,
      results_export = results_df,
      strategies_data = strategies_df,
      grouping_vars = grouping_vars,
      treatment_var = treatment_var
    ))
    
  }, error = function(e) {
    # Fallback a método anterior si export_results falla
    warning(paste("export_results falló, usando método básico:", e$message))
    
    factors_df <- experiment$factors
    strategies_df <- strategies$calls
    
    # Agregar Track ID si no existe
    if (!"Track_ID" %in% names(strategies_df)) {
      strategies_df$Track_ID <- rownames(strategies_df)
    }
    
    # Merge datos
    if ("_TrackID" %in% names(factors_df)) {
      merged_data <- merge(factors_df, strategies_df, 
                          by.x = "_TrackID", by.y = "Track_ID", all = TRUE)
    } else {
      merged_data <- cbind(factors_df, strategies_df)
    }
    
    # Clasificar estrategias
    merged_data <- classify_strategies(merged_data)
    
    return(list(
      merged_data = merged_data,
      grouping_vars = grouping_vars,
      treatment_var = treatment_var
    ))
  })
}

# Función para clasificar estrategias según categorías cognitivas
classify_strategies <- function(data) {
  
  # Mapeo de estrategias individuales a categorías
  strategy_mapping <- list(
    "allocentric" = c("directed search", "corrected path", "direct path"),
    "egocentric" = c("thigmotaxis", "circling", "random path", "scanning", "chaining"),
    "perseverance" = c("perseverance")
  )
  
  # Agregar columna de clasificación
  data$strategy_category <- NA
  
  for (category in names(strategy_mapping)) {
    mask <- data$name %in% strategy_mapping[[category]]
    data$strategy_category[mask] <- category
  }
  
  # Factorizar para análisis
  data$strategy_category <- factor(data$strategy_category, 
                                  levels = c("egocentric", "allocentric", "perseverance"))
  
  return(data)
}

# Función para crear datos longitudinales
create_longitudinal_data <- function(data, grouping_vars, treatment_var, day_var) {
  
  if (is.null(grouping_vars) || is.null(treatment_var) || is.null(day_var)) {
    return(NULL)
  }
  
  # Crear resumen por día y grupo
  group_cols <- c(day_var, grouping_vars, treatment_var, "strategy_category")
  
  # Filtrar columnas que existen
  existing_cols <- group_cols[group_cols %in% names(data)]
  
  if (length(existing_cols) < 3) {
    warning("No se pueden crear datos longitudinales con las variables especificadas")
    return(NULL)
  }
  
  # Resumir por grupo
  summary_data <- data %>%
    dplyr::group_by(across(all_of(existing_cols))) %>%
    dplyr::summarise(
      count = n(),
      avg_confidence = mean(confidence, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(summary_data)
}

# Función para identificar formato de track automáticamente
identify_track_format_safe <- function(file_path) {
  tryCatch({
    format <- Rtrack::identify_track_format(file_path)
    return(format)
  }, error = function(e) {
    # Si falla, intentar formatos comunes
    common_formats <- c("ethovision.3.csv", "raw.csv", "raw.tab")
    
    for (format in common_formats) {
      tryCatch({
        # Intentar leer con cada formato
        test_read <- utils::read.table(file_path, header = TRUE, sep = ",", nrows = 5)
        if (ncol(test_read) >= 3) {
          return(format)
        }
      }, error = function(e2) {
        # Continuar con el siguiente formato
      })
    }
    
    return("raw.csv")  # Formato por defecto
  })
}

# Función para validar estructura de archivos de experimento
validate_experiment_structure <- function(experiment_data) {
  required_columns <- c("_TrackID", "_TargetID", "_Day", "_Trial", 
                       "_Arena", "_TrackFile", "_TrackFileFormat")
  
  missing_columns <- setdiff(required_columns, names(experiment_data))
  
  if (length(missing_columns) > 0) {
    stop(paste("Faltan columnas requeridas:", paste(missing_columns, collapse = ", ")))
  }
  
  # Validar que hay datos
  if (nrow(experiment_data) == 0) {
    stop("El archivo de experimento está vacío")
  }
  
  # Validar que los archivos de track existen
  track_files <- unique(experiment_data$`_TrackFile`)
  if (length(track_files) == 0) {
    stop("No se especificaron archivos de tracks")
  }
  
  return(TRUE)
}

# Función para preparar datos para análisis estadístico
prepare_statistical_data <- function(processed_data, response_var = "count", 
                                   fixed_effects = NULL, random_effects = NULL) {
  
  if (is.null(processed_data$longitudinal_data)) {
    warning("No hay datos longitudinales disponibles para análisis estadístico")
    return(NULL)
  }
  
  data <- processed_data$longitudinal_data
  
  # Preparar fórmula para modelo
  if (is.null(fixed_effects)) {
    fixed_effects <- c(processed_data$treatment_var, processed_data$day_var, "strategy_category")
  }
  
  if (is.null(random_effects)) {
    random_effects <- "_TargetID"  # Sujeto como efecto aleatorio
  }
  
  # Crear fórmula
  formula_str <- paste(response_var, "~", paste(fixed_effects, collapse = " * "))
  
  if (!is.null(random_effects) && random_effects %in% names(data)) {
    formula_str <- paste(formula_str, "+ (1 |", random_effects, ")")
  }
  
  return(list(
    data = data,
    formula = as.formula(formula_str),
    response_var = response_var,
    fixed_effects = fixed_effects,
    random_effects = random_effects
  ))
}

# Función para crear archivo de ejemplo de experimento
create_example_experiment <- function() {
  example_data <- data.frame(
    `_TrackID` = paste0("Track_", 1:20),
    `_TargetID` = rep(paste0("Subject_", 1:5), each = 4),
    `_Day` = rep(1:4, 5),
    `_Trial` = rep(1:4, 5),
    `_Arena` = "Arena_SW.txt",
    `_TrackFile` = paste0("Track_", 1:20, ".csv"),
    `_TrackFileFormat` = "ethovision.3.csv",
    Tratamiento = rep(c("Control", "Experimental"), each = 10),
    Estres = rep(c("No", "Si"), times = 10),
    check.names = FALSE
  )
  
  return(example_data)
}
