# Funciones auxiliares para generar gráficos
# Contiene funciones para crear mapas de densidad y análisis de estrategias

# Función para crear mapas de densidad agrupados
create_density_maps <- function(experiment_data, grouping_var, days_filter = NULL,
                               color_palette = "viridis", resolution = 900,
                               color_levels = 300, show_legend = FALSE) {
  
  # Filtrar datos por días si se especifica
  if (!is.null(days_filter)) {
    day_col <- names(experiment_data$factors)[grepl("^_Day", names(experiment_data$factors))][1]
    if (!is.null(day_col)) {
      filter_mask <- experiment_data$factors[[day_col]] %in% days_filter
      metrics_filtered <- experiment_data$metrics[filter_mask]
      factors_filtered <- experiment_data$factors[filter_mask, ]
    } else {
      metrics_filtered <- experiment_data$metrics
      factors_filtered <- experiment_data$factors
    }
  } else {
    metrics_filtered <- experiment_data$metrics
    factors_filtered <- experiment_data$factors
  }
  
  # Obtener grupos únicos
  if (grouping_var %in% names(factors_filtered)) {
    groups <- unique(factors_filtered[[grouping_var]])
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
  
  # Crear plots para cada grupo
  plots_list <- list()
  
  for (group in groups) {
    group_mask <- factors_filtered[[grouping_var]] == group
    group_metrics <- metrics_filtered[group_mask]
    
    if (length(group_metrics) > 0) {
      tryCatch({
        # Crear plot de densidad
        p <- Rtrack::plot_density(
          group_metrics,
          title = paste(grouping_var, ":", group),
          col = color_func(color_levels),
          resolution = resolution,
          feature.col = "#E87FB0",
          feature.lwd = 4,
          legend = show_legend
        )
        
        plots_list[[as.character(group)]] <- p
        
      }, error = function(e) {
        warning(paste("Error creando mapa para grupo", group, ":", e$message))
      })
    }
  }
  
  if (length(plots_list) == 0) {
    stop("No se pudieron crear mapas de densidad")
  }
  
  # Combinar plots en una sola figura
  if (length(plots_list) > 1) {
    # Configurar layout de múltiples plots
    n_plots <- length(plots_list)
    n_cols <- ceiling(sqrt(n_plots))
    n_rows <- ceiling(n_plots / n_cols)
    
    # Configurar el layout
    par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 3, 2))
    
    # Crear función que genere todos los plots
    combined_plot <- function() {
      for (i in seq_along(plots_list)) {
        # Recrear cada plot (necesario porque plot_density no retorna objeto de plot)
        group <- names(plots_list)[i]
        group_mask <- factors_filtered[[grouping_var]] == group
        group_metrics <- metrics_filtered[group_mask]
        
        if (length(group_metrics) > 0) {
          Rtrack::plot_density(
            group_metrics,
            title = paste(grouping_var, ":", group),
            col = color_func(color_levels),
            resolution = resolution,
            feature.col = "#E87FB0",
            feature.lwd = 4,
            legend = show_legend
          )
        }
      }
      par(mfrow = c(1, 1))  # Resetear layout
    }
    
    return(combined_plot)
    
  } else {
    # Un solo plot
    group <- names(plots_list)[1]
    group_mask <- factors_filtered[[grouping_var]] == group
    group_metrics <- metrics_filtered[group_mask]
    
    single_plot <- function() {
      Rtrack::plot_density(
        group_metrics,
        title = paste(grouping_var, ":", group),
        col = color_func(color_levels),
        resolution = resolution,
        feature.col = "#E87FB0",
        feature.lwd = 4,
        legend = show_legend
      )
    }
    
    return(single_plot)
  }
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
  
  # Crear gráfico de barras apiladas
  p <- ggplot2::ggplot(summary_data, 
                      ggplot2::aes_string(x = day_col, y = "proportion", 
                                         fill = "estrategias_hipo")) +
    ggplot2::geom_bar(stat = "identity", position = "fill", color = "black") +
    ggplot2::facet_wrap(stats::as.formula(paste("~", grouping_var))) +
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
  
  # Crear gráfico de líneas
  p <- ggplot2::ggplot(summary_data, 
                      ggplot2::aes_string(x = day_col, y = "count", 
                                         color = "name", linetype = "estrategias_hipo")) +
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::facet_wrap(stats::as.formula(paste("~", grouping_var))) +
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
