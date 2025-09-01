# Funciones auxiliares para generar gráficos
# Contiene funciones para crear mapas de densidad y análisis de estrategias

# Función para crear mapas de densidad agrupados
create_density_maps <- function(experiment_data, grouping_var, days_filter = NULL,
                               arena_filter = NULL, color_palette = "viridis",
                               resolution = 900, color_levels = 300,
                               show_legend = FALSE, overlay_goals = FALSE) {

  # Verificaciones iniciales
  if (is.null(experiment_data) || is.null(experiment_data$factors)) {
    return("No hay datos disponibles para generar mapas")
  }
  
  if (!grouping_var %in% names(experiment_data$factors)) {
    return(paste("Variable de agrupación", grouping_var, "no encontrada"))
  }
  
  if (nrow(experiment_data$factors) == 0) {
    return("No hay suficientes datos después del filtrado")
  }
  
  # Verificar paths - validación robusta
  if (is.null(experiment_data$paths)) {
    return("No hay datos de trayectorias disponibles")
  }
  
  if (is.list(experiment_data$paths) && length(experiment_data$paths) == 0) {
    return("Lista de trayectorias vacía")
  }

  # Obtener grupos únicos
  groups <- unique(experiment_data$factors[[grouping_var]])
  groups <- groups[!is.na(groups)]
  
  if (length(groups) == 0) {
    return("No hay grupos válidos para la variable seleccionada")
  }

  # Crear función de ploteo
  plot_function <- function() {
    # Guardar parámetros gráficos actuales
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    
    # Configuración de subplots
    n_groups <- length(groups)
    if (n_groups <= 2) {
      par(mfrow = c(1, n_groups))
    } else if (n_groups <= 4) {
      par(mfrow = c(2, 2))
    } else {
      par(mfrow = c(ceiling(n_groups/2), 2))
    }
    par(mar = c(4, 4, 2, 1))

    for (group in groups) {
      # Obtener IDs de tracks para este grupo
      group_tracks <- experiment_data$factors$`_TrackID`[
        experiment_data$factors[[grouping_var]] == group
      ]
      
      if (length(group_tracks) == 0) {
        plot.new()
        title(paste("Grupo:", group, "(sin datos)"))
        next
      }

      # Extraer paths para estos tracks
      if (is.list(experiment_data$paths)) {
        # Si paths tiene nombres, usar nombres
        if (!is.null(names(experiment_data$paths))) {
          group_paths <- experiment_data$paths[names(experiment_data$paths) %in% group_tracks]
        } else {
          # Intentar extraer _TrackID de cada path
          group_paths <- list()
          for (i in seq_along(experiment_data$paths)) {
            p <- experiment_data$paths[[i]]
            id <- NULL
            if (!is.null(p$`_TrackID`)) {
              id <- p$`_TrackID`
            } else if (!is.null(attr(p, "_TrackID"))) {
              id <- attr(p, "_TrackID")
            }
            if (!is.null(id) && id %in% group_tracks) {
              group_paths[[length(group_paths) + 1]] <- p
            }
          }
        }
      } else {
        # Si paths no es una lista, probablemente sea un error
        plot.new()
        title(paste("Grupo:", group, "(estructura de paths inválida)"))
        next
      }

      if (length(group_paths) == 0) {
        plot.new()
        title(paste("Grupo:", group, "(sin trayectorias)"))
        next
      }

      # Extraer coordenadas X,Y de todos los paths
      all_x <- all_y <- numeric(0)
      for (path in group_paths) {
        if (!is.null(path$x) && !is.null(path$y)) {
          all_x <- c(all_x, path$x)
          all_y <- c(all_y, path$y)
        }
      }

      # Eliminar NAs
      valid_points <- !is.na(all_x) & !is.na(all_y)
      all_x <- all_x[valid_points]
      all_y <- all_y[valid_points]

      if (length(all_x) == 0) {
        plot.new()
        title(paste("Grupo:", group, "(sin coordenadas válidas)"))
        next
      }

      # Generar mapa de densidad con manejo de errores
      tryCatch({
        # Cargar librería en modo silencioso
        suppressWarnings(library(MASS))
        
        # Calcular densidad kernel (kde2d)
        n_points <- max(30, min(resolution/10, 512))
        dens <- suppressWarnings(
          MASS::kde2d(all_x, all_y, n = n_points)
        )
        
        # Dibujar mapa de densidad
        image(dens, col = hcl.colors(color_levels, color_palette), 
              main = paste("Densidad -", group),
              xlab = "X", ylab = "Y")
        
        # Añadir contornos
        contour(dens, add = TRUE, col = "white", lwd = 0.5)
        
      }, error = function(e) {
        # Fallback: dibujar las trayectorias como puntos
        plot(all_x, all_y, pch = 16, cex = 0.3, col = rgb(0, 0, 1, 0.3),
             main = paste("Trayectorias -", group),
             xlab = "X", ylab = "Y")
      })
    }
  }

  return(plot_function)
}

# Función para crear análisis de estrategias
create_strategy_analysis <- function(data, grouping_var, days_filter = NULL, 
                                   probe_filter = NULL, arena_filter = NULL) {
  
  # Aplicar filtros
  if (!is.null(probe_filter) && probe_filter != "Todos") {
    probe_value <- probe_filter == "Solo pruebas (Probe = TRUE)"
    data <- data[data$Probe == probe_value, ]
  }
  
  if (!is.null(arena_filter) && length(arena_filter) > 0 && arena_filter[1] != "") {
    selected_arenas <- arena_filter
    selected_arenas_pattern <- paste0(paste0(selected_arenas, 
                                           ifelse(grepl("\\.txt$", selected_arenas), "", ".txt")),
                                     collapse = "|")
    data <- data[grepl(selected_arenas_pattern, data$`_Arena`), ]
  }
  
  # Continuar con el procesamiento...
}

# Función para clasificar estrategias para plotting
classify_strategies_for_plot <- function(data) {
  
  # Clasificar según las categorías cognitivas
  data$estrategias_hipo <- NA
  
  # Estrategias alocéntricas (hipocampo-dependientes)
  allocentric_strategies <- c("directed search", "corrected path", "direct path")
  data$estrategias_hipo[data$name %in% allocentric_strategies] <- "Alocéntricas"
  
  # Estrategias egocéntricas (hipocampo-independientes)  
  egocentric_strategies <- c("thigmotaxis", "circling", "random path", "scanning", "chaining")
  data$estrategias_hipo[data$name %in% egocentric_strategies] <- "Egocéntricas"
  
  # Perseverancia
  data$estrategias_hipo[data$name == "perseverance"] <- "Perseverancia"
  
  # Factorizar
  data$estrategias_hipo <- factor(data$estrategias_hipo, 
                                 levels = c("Egocéntricas", "Alocéntricas", "Perseverancia"))
  
  return(data)
}

# Función para crear gráfico de estrategias agrupadas
create_grouped_strategy_plot <- function(data, grouping_var) {
  
  # Crear resumen de datos
  day_col <- names(data)[grepl("^_Day", names(data))][1]
  
  if (is.null(day_col)) {
    stop("No se encontró columna de día")
  }
  
  # Resumir por grupo, día y tipo de estrategia
  summary_data <- data %>%
    dplyr::filter(!is.na(estrategias_hipo)) %>%
    dplyr::group_by(!!dplyr::sym(grouping_var), !!dplyr::sym(day_col), estrategias_hipo) %>%
    dplyr::summarise(
      count = dplyr::n(),
      rate = dplyr::n() / 4,  # Asumiendo 4 trials por día
      .groups = "drop"
    ) %>%
    dplyr::group_by(!!dplyr::sym(grouping_var), !!dplyr::sym(day_col)) %>%
    dplyr::mutate(
      total = sum(count),
      proportion = count / total
    )
  
  # Crear columnas seguras para ggplot (evita problemas con nombres no sintácticos)
  summary_data$DayVar <- summary_data[[day_col]]
  summary_data$GroupVar <- summary_data[[grouping_var]]
  
  # Crear gráfico de barras apiladas
  p <- ggplot2::ggplot(summary_data, 
                      ggplot2::aes(x = DayVar, y = proportion, fill = estrategias_hipo)) +
    ggplot2::geom_bar(stat = "identity", position = "fill", color = "black") +
    ggplot2::facet_wrap(~ GroupVar) +
    ggplot2::scale_fill_manual(values = c("Egocéntricas" = "#EA5455", 
                                         "Alocéntricas" = "#2D4059",
                                         "Perseverancia" = "#F07B3F")) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "top",
      strip.background = ggplot2::element_rect(fill = "white", color = "gray"),
      strip.text = ggplot2::element_text(face = "bold")
    ) +
    ggplot2::labs(
      x = "Día",
      y = "Proporción de Estrategias",
      fill = "Tipo de Estrategia",
      title = "Distribución de Estrategias de Búsqueda"
    )
  
  return(p)
}

# Función para crear gráfico de estrategias individuales
create_individual_strategy_plot <- function(data, grouping_var) {
  
  day_col <- names(data)[grepl("^_Day", names(data))][1]
  
  # Crear resumen por estrategia individual
  summary_data <- data %>%
    dplyr::filter(!is.na(name)) %>%
    dplyr::group_by(!!dplyr::sym(grouping_var), !!dplyr::sym(day_col), name, estrategias_hipo) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop")
  
  # Columnas seguras para ggplot
  summary_data$DayVar <- summary_data[[day_col]]
  summary_data$GroupVar <- summary_data[[grouping_var]]
  
  # Crear gráfico de líneas
  p <- ggplot2::ggplot(summary_data, 
                      ggplot2::aes(x = DayVar, y = count, 
                                   color = name, linetype = estrategias_hipo)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::facet_wrap(~ GroupVar) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "vertical"
    ) +
    ggplot2::labs(
      x = "Día",
      y = "Número de Estrategias",
      color = "Estrategia",
      linetype = "Categoría",
      title = "Evolución de Estrategias Individuales"
    )
  
  return(p)
}

# Función para análisis estadístico básico
perform_strategy_statistics <- function(data, grouping_var) {
  
  day_col <- names(data)[grepl("^_Day", names(data))][1]
  
  if (is.null(day_col) || !grouping_var %in% names(data)) {
    return(list(summary = "Análisis estadístico no disponible"))
  }
  
  # Crear tabla de contingencia
  tryCatch({
    
    # Resumir datos
    summary_stats <- data %>%
      dplyr::filter(!is.na(estrategias_hipo)) %>%
      dplyr::group_by(!!dplyr::sym(grouping_var), estrategias_hipo) %>%
      dplyr::summarise(
        count = dplyr::n(),
        avg_confidence = mean(confidence, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Chi-square test si hay suficientes datos
    contingency_table <- table(data[[grouping_var]], data$estrategias_hipo)
    
    if (all(contingency_table >= 5)) {
      chi_test <- stats::chisq.test(contingency_table)
      
      return(list(
        summary_stats = summary_stats,
        contingency_table = contingency_table,
        chi_square = chi_test,
        interpretation = "Test chi-cuadrado significativo indica diferencias en estrategias entre grupos"
      ))
    } else {
      return(list(
        summary_stats = summary_stats,
        contingency_table = contingency_table,
        note = "Datos insuficientes para test chi-cuadrado"
      ))
    }
    
  }, error = function(e) {
    return(list(error = paste("Error en análisis estadístico:", e$message)))
  })
}

# Función para customizar strips de facetas (según tu ejemplo)
customize_facet_strips <- function(plot_obj, strip_colors) {
  # Esta función replicaría tu función customize_facet_strips
  # Por ahora retorna el plot sin modificaciones
  return(plot_obj)
}

# Función para crear análisis completo con resultados exportados
create_comprehensive_analysis <- function(results_data, variable, grouping_var, 
                                        days_filter = NULL, show_error_bars = TRUE,
                                        log_transform = FALSE) {
  
  # Filtrar datos por días
  if (!is.null(days_filter)) {
    day_col <- names(results_data)[grepl("^_Day", names(results_data))][1]
    if (!is.null(day_col)) {
      results_data <- results_data[results_data[[day_col]] %in% days_filter, ]
    }
  }
  
  # Verificar que tenemos las variables necesarias
  if (!variable %in% names(results_data)) {
    stop(paste("Variable", variable, "no encontrada en los datos"))
  }
  
  if (!grouping_var %in% names(results_data)) {
    stop(paste("Variable de agrupación", grouping_var, "no encontrada"))
  }
  
  # Transformación logarítmica si se solicita
  if (log_transform) {
    results_data[[variable]] <- log10(results_data[[variable]] + 1)
    y_label <- paste("log10(", variable, " + 1)")
  } else {
    y_label <- variable
  }
  
  # Preparar datos para el gráfico con columnas seguras (evita problemas con nombres no sintácticos)
  day_col <- names(results_data)[grepl("^_Day", names(results_data))][1]
  results_data$YVar <- results_data[[variable]]
  results_data$GroupVar <- results_data[[grouping_var]]
  has_day <- !is.null(day_col)
  if (has_day) {
    results_data$DayVar <- results_data[[day_col]]
    # Gráfico con días como eje X
    p <- ggplot2::ggplot(results_data, ggplot2::aes(x = DayVar, y = YVar, color = GroupVar, fill = GroupVar))
    if (show_error_bars) {
      p <- p + ggplot2::stat_summary(fun = mean, geom = "point", size = 3, position = ggplot2::position_dodge(0.3)) +
               ggplot2::stat_summary(fun = mean, geom = "line", size = 1, position = ggplot2::position_dodge(0.3), ggplot2::aes(group = GroupVar)) +
               ggplot2::stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, position = ggplot2::position_dodge(0.3))
    } else {
      p <- p + ggplot2::stat_summary(fun = mean, geom = "point", size = 3, position = ggplot2::position_dodge(0.3)) +
               ggplot2::stat_summary(fun = mean, geom = "line", size = 1, position = ggplot2::position_dodge(0.3), ggplot2::aes(group = GroupVar))
    }
    p <- p + ggplot2::labs(x = "Día", y = y_label, color = grouping_var, fill = grouping_var)
  } else {
    # Gráfico de barras simple sin días
    p <- ggplot2::ggplot(results_data, ggplot2::aes(x = GroupVar, y = YVar, fill = GroupVar))
    if (show_error_bars) {
      p <- p + ggplot2::stat_summary(fun = mean, geom = "bar", alpha = 0.7) +
               ggplot2::stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2)
    } else {
      p <- p + ggplot2::stat_summary(fun = mean, geom = "bar", alpha = 0.7)
    }
    p <- p + ggplot2::labs(x = grouping_var, y = y_label, fill = grouping_var)
  }
  
  # Tema y estilo
  p <- p + ggplot2::theme_minimal() +
           ggplot2::theme(
             legend.position = "top",
             strip.background = ggplot2::element_rect(fill = "white", color = "gray"),
             strip.text = ggplot2::element_text(face = "bold"),
             axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
           ) +
           ggplot2::scale_color_viridis_d() +
           ggplot2::scale_fill_viridis_d()
  
  return(p)
}

# Función auxiliar para calcular mean_se
mean_se <- function(x) {
  n <- length(x[!is.na(x)])
  mean_val <- mean(x, na.rm = TRUE)
  se_val <- sd(x, na.rm = TRUE) / sqrt(n)
  
  data.frame(
    y = mean_val,
    ymin = mean_val - se_val,
    ymax = mean_val + se_val
  )
}

# Función para ejecutar análisis estadísticos
perform_statistical_analysis <- function(data, variable, grouping_var, test_type = "anova") {
  
  # Verificar que tenemos datos válidos
  if (!variable %in% names(data) || !grouping_var %in% names(data)) {
    stop("Variables no encontradas en los datos")
  }
  
  # Remover NAs
  clean_data <- data[!is.na(data[[variable]]) & !is.na(data[[grouping_var]]), ]
  
  if (nrow(clean_data) == 0) {
    stop("No hay datos válidos para el análisis")
  }
  
  # Crear fórmula base
  formula_str <- paste(variable, "~", grouping_var)
  formula_obj <- as.formula(formula_str)
  
  # Ejecutar la prueba seleccionada
  result <- tryCatch({
    switch(test_type,
           "anova" = {
             if (length(unique(clean_data[[grouping_var]])) > 2) {
               aov_result <- aov(formula_obj, data = clean_data)
               list(
                 test = "ANOVA",
                 result = summary(aov_result),
                 model = aov_result,
                 formula = formula_obj
               )
             } else {
               t_result <- t.test(formula_obj, data = clean_data)
               list(
                 test = "t-test (2 grupos)",
                 result = t_result,
                 model = t_result,
                 formula = formula_obj
               )
             }
           },
           "ttest" = {
             t_result <- t.test(formula_obj, data = clean_data)
             list(
               test = "t-test",
               result = t_result,
               model = t_result,
               formula = formula_obj
             )
           },
           "kruskal" = {
             k_result <- kruskal.test(formula_obj, data = clean_data)
             list(
               test = "Kruskal-Wallis",
               result = k_result,
               model = k_result,
               formula = formula_obj
             )
           },
           "wilcox" = {
             w_result <- wilcox.test(formula_obj, data = clean_data)
             list(
               test = "Mann-Whitney U",
               result = w_result,
               model = w_result,
               formula = formula_obj
             )
           },
           "poisson" = {
             # Armar modelos Poisson con y sin interacción si hay día
             day_col <- names(clean_data)[grepl("^_Day", names(clean_data))][1]
             has_day <- !is.null(day_col) && length(unique(clean_data[[day_col]])) > 1
             # Modelo sin interacción
             f_null <- if (has_day) as.formula(paste(variable, "~", grouping_var, "+", day_col)) else formula_obj
             # Modelo con interacción
             f_full <- if (has_day) as.formula(paste(variable, "~", grouping_var, "*", day_col)) else as.formula(paste(variable, "~", grouping_var))

             # Verificar no-negativo
             if (any(clean_data[[variable]] < 0, na.rm = TRUE)) {
               stop("La variable tiene valores negativos; Poisson requiere conteos (>= 0)")
             }
             # Ajustar GLM Poisson
             m_null <- glm(f_null, data = clean_data, family = poisson(link = "log"))
             m_full <- glm(f_full, data = clean_data, family = poisson(link = "log"))
             lrt <- tryCatch({
               anova(m_null, m_full, test = "LRT")
             }, error = function(e) NULL)
             list(
               test = if (has_day) "Poisson GLM con/ sin interacción (LRT)" else "Poisson GLM",
               result = summary(m_full),
               model = m_full,
               model_null = if (has_day) m_null else NULL,
               lrt = lrt,
               formula = f_full,
               family = "Poisson (log)"
             )
           }
    )
  }, error = function(e) {
    list(
      test = paste("Error en", test_type),
      result = paste("Error:", e$message),
      model = NULL,
      formula = formula_obj
    )
  })
  
  return(result)
}
