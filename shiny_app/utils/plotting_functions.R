# Funciones auxiliares para generar gráficos
# Contiene funciones para crear mapas de densidad y análisis de estrategias

# Función para crear mapas de densidad agrupados
create_density_maps <- function(experiment_data, grouping_var, days_filter = NULL,
                               color_palette = "viridis", resolution = 900,
                               color_levels = 300, show_legend = FALSE, arena_filter = NULL) {
  
  # Filtrar datos por días y arena si se especifica
  filter_mask <- rep(TRUE, length(experiment_data$metrics))
  
  if (!is.null(days_filter)) {
    day_col <- names(experiment_data$factors)[grepl("^_Day", names(experiment_data$factors))][1]
    if (!is.null(day_col)) {
      filter_mask <- filter_mask & (experiment_data$factors[[day_col]] %in% days_filter)
    }
  }
  
  if (!is.null(arena_filter)) {
    arena_col <- names(experiment_data$factors)[grepl("^_Arena", names(experiment_data$factors))][1]
    if (!is.null(arena_col)) {
      filter_mask <- filter_mask & (experiment_data$factors[[arena_col]] %in% arena_filter)
    }
  }
  
  metrics_filtered <- experiment_data$metrics[filter_mask]
  factors_filtered <- experiment_data$factors[filter_mask, ]
  
  # Verificar que tenemos datos
  if (length(metrics_filtered) == 0) {
    stop("No hay datos después del filtrado")
  }
  
  # Obtener grupos únicos
  if (grouping_var %in% names(factors_filtered)) {
    groups <- unique(factors_filtered[[grouping_var]])
    groups <- groups[!is.na(groups)]
  } else {
    stop(paste("Variable de agrupación", grouping_var, "no encontrada"))
  }
  
  # Configurar paleta de colores
  color_func <- switch(color_palette,
    "viridis" = viridis::viridis,
    "plasma" = viridis::plasma,
    "inferno" = viridis::inferno,
    "magma" = viridis::magma,
    "cividis" = viridis::cividis,
    viridis::viridis
  )
  
  # Crear función que genere todos los plots
  combined_plot <- function() {
    # Determinar layout
    n_groups <- length(groups)
    if (n_groups > 1) {
      n_cols <- min(3, ceiling(sqrt(n_groups)))
      n_rows <- ceiling(n_groups / n_cols)
      par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 3, 2))
    } else {
      par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))
    }
    
    plots_created <- 0
    
    for (group in groups) {
      group_mask <- factors_filtered[[grouping_var]] == group
      group_metrics <- metrics_filtered[group_mask]
      
      if (length(group_metrics) > 0) {
        tryCatch({
          # Verificar si hay múltiples arenas en este grupo
          group_arenas <- unique(factors_filtered[group_mask, ]$`_Arena`)
          
          if (length(group_arenas) > 1) {
            # Advertir sobre múltiples arenas pero continuar
            title_text <- paste(grouping_var, ":", group, "(múltiples arenas)")
          } else {
            title_text <- paste(grouping_var, ":", group)
          }
          
          # Crear plot de densidad
          Rtrack::plot_density(
            group_metrics,
            title = title_text,
            col = color_func(color_levels),
            resolution = resolution,
            feature.col = "#E87FB0",
            feature.lwd = 4,
            legend = show_legend
          )
          
          plots_created <- plots_created + 1
          
        }, error = function(e) {
          # Crear un plot vacío con mensaje de error
          plot.new()
          text(0.5, 0.5, paste("Error:", e$message), cex = 0.8, col = "red")
          title(paste("Error -", grouping_var, ":", group))
        })
      } else {
        # Crear un plot vacío si no hay datos
        plot.new()
        text(0.5, 0.5, "Sin datos", cex = 1, col = "gray")
        title(paste(grouping_var, ":", group, "- Sin datos"))
      }
    }
    
    # Resetear layout
    par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))
    
    if (plots_created == 0) {
      stop("No se pudieron crear mapas de densidad")
    }
  }
  
  return(combined_plot)
}

# Función para crear análisis de estrategias
create_strategy_analysis <- function(strategies_data, experiment_data, grouping_var,
                                   days_filter = NULL, show_individual = FALSE) {
  
  # Combinar datos
  factors_df <- experiment_data$factors
  strategies_df <- strategies_data$calls
  
  # Merge datos
  if ("_TrackID" %in% names(factors_df)) {
    if (!"Track_ID" %in% names(strategies_df)) {
      strategies_df$Track_ID <- rownames(strategies_df)
    }
    merged_data <- merge(factors_df, strategies_df, 
                        by.x = "_TrackID", by.y = "Track_ID", all = TRUE)
  } else {
    merged_data <- cbind(factors_df, strategies_df)
  }
  
  # Filtrar por días
  if (!is.null(days_filter)) {
    day_col <- names(merged_data)[grepl("^_Day", names(merged_data))][1]
    if (!is.null(day_col)) {
      merged_data <- merged_data[merged_data[[day_col]] %in% days_filter, ]
    }
  }
  
  # Clasificar estrategias
  merged_data <- classify_strategies_for_plot(merged_data)
  
  # Crear gráfico de estrategias
  if (show_individual) {
    plot_obj <- create_individual_strategy_plot(merged_data, grouping_var)
  } else {
    plot_obj <- create_grouped_strategy_plot(merged_data, grouping_var)
  }
  
  # Análisis estadístico básico
  stats_results <- perform_strategy_statistics(merged_data, grouping_var)
  
  return(list(
    plots = plot_obj,
    stats = stats_results,
    data = merged_data
  ))
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
