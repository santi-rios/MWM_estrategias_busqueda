# Módulo unificado para filtros
# Centraliza todos los filtros para asegurar consistencia

filterUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "🔍 Filtros de Datos",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        collapsed = FALSE,
        
        fluidRow(
          column(4,
            selectInput(ns("probe_filter"), "Prueba (Probe):", 
                        choices = c("Todos" = "all", 
                                   "Solo entrenamiento (Probe = FALSE)" = "false", 
                                   "Solo pruebas (Probe = TRUE)" = "true"),
                        selected = "all")
          ),
          column(4,
            uiOutput(ns("arena_selector"))
          ),
          column(4,
            uiOutput(ns("day_selector"))
          )
        ),
        
        fluidRow(
          column(4,
            uiOutput(ns("target_id_selector")) 
          ),
          column(4,
            uiOutput(ns("custom_filter1"))
          ),
          column(4,
            uiOutput(ns("custom_filter2"))
          )
        ),
        
        br(),
        div(
          style = "text-align: center;",
          actionButton(ns("apply_filters"), 
                       "✓ Aplicar Filtros", 
                       class = "btn-success"),
          actionButton(ns("clear_filters"), 
                       "✗ Borrar Filtros", 
                       class = "btn-danger")
        ),
        
        br(),
        uiOutput(ns("filter_status"))
      )
    )
  )
}

filterServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialize filters state
    filters_state <- reactiveVal(list(
      probe = "all",
      arenas = character(0),
      days = numeric(0),
      target_ids = character(0),
      custom1 = list(field = NULL, values = NULL),
      custom2 = list(field = NULL, values = NULL)
    ))
    
    # Auto-detect available filter fields
    observe({
      req(values$processed_data)
      
      # Get factors data
      factors <- values$processed_data$factors
      
      # Setup Arena selector
      if ("_Arena" %in% names(factors)) {
        arenas <- unique(na.omit(factors$`_Arena`))
        arenas <- gsub("\\.txt$", "", arenas)  # Remove .txt extension
        output$arena_selector <- renderUI({
          selectInput(ns("arena_filter"), "Arena:", 
                     choices = c("Todas" = "", arenas),
                     multiple = TRUE)
        })
      } else {
        output$arena_selector <- renderUI({
          div(class = "text-muted", "Arena: no disponible")
        })
      }
      
      # Setup Day selector
      if ("_Day" %in% names(factors)) {
        days <- sort(unique(na.omit(factors$`_Day`)))
        output$day_selector <- renderUI({
          selectInput(ns("day_filter"), "Día:", 
                     choices = days,
                     multiple = TRUE)
        })
      } else {
        output$day_selector <- renderUI({
          div(class = "text-muted", "Día: no disponible")
        })
      }
      
      # Setup Target ID selector
      if ("_TargetID" %in% names(factors)) {
        target_ids <- sort(unique(na.omit(factors$`_TargetID`)))
        output$target_id_selector <- renderUI({
          selectInput(ns("target_id_filter"), "ID Sujeto:", 
                     choices = c("Todos" = "", target_ids),
                     multiple = TRUE)
        })
      } else {
        output$target_id_selector <- renderUI({
          div(class = "text-muted", "ID Sujeto: no disponible")
        })
      }
      
      # Setup custom filters based on non-standard columns
      custom_fields <- names(factors)[!grepl("^_", names(factors)) & 
                                     !names(factors) %in% c("Probe")]
      
      if (length(custom_fields) > 0) {
        # First custom filter
        output$custom_filter1 <- renderUI({
          if (length(custom_fields) >= 1) {
            field <- custom_fields[1]
            field_values <- sort(unique(na.omit(factors[[field]])))
            tagList(
              h6(paste("Campo:", field)),
              selectInput(ns("custom1_values"), "Valores:", choices = field_values, multiple = TRUE)
            )
          }
        })
        
        # Second custom filter (if available)
        output$custom_filter2 <- renderUI({
          if (length(custom_fields) >= 2) {
            field <- custom_fields[2]
            field_values <- sort(unique(na.omit(factors[[field]])))
            tagList(
              h6(paste("Campo:", field)),
              selectInput(ns("custom2_values"), "Valores:", choices = field_values, multiple = TRUE)
            )
          }
        })
      }
    })
    
    # Apply filters button
    observeEvent(input$apply_filters, {
      new_filters <- list(
        probe = input$probe_filter,
        arenas = if (!is.null(input$arena_filter)) input$arena_filter else character(0),
        days = if (!is.null(input$day_filter)) input$day_filter else numeric(0),
        target_ids = if (!is.null(input$target_id_filter)) input$target_id_filter else character(0)
      )
      
      # Add custom filters if present
      custom_fields <- names(values$processed_data$factors)[!grepl("^_", names(values$processed_data$factors)) & 
                                                            !names(values$processed_data$factors) %in% c("Probe")]
      
      if (length(custom_fields) >= 1 && !is.null(input$custom1_values)) {
        new_filters$custom1 <- list(
          field = custom_fields[1],
          values = input$custom1_values
        )
      }
      
      if (length(custom_fields) >= 2 && !is.null(input$custom2_values)) {
        new_filters$custom2 <- list(
          field = custom_fields[2],
          values = input$custom2_values
        )
      }
      
      # Update filters state
      filters_state(new_filters)
      
      # Apply filters to global values
      values$active_filters <- new_filters
      
      # Filter the data
      filtered_data <- apply_filters(values$processed_data, new_filters)
      values$filtered_data <- filtered_data
      
      # Update strategies to match filtered data
      if (!is.null(values$strategies) && !is.null(filtered_data)) {
        track_ids <- unique(filtered_data$factors$`_TrackID`)
        if (length(track_ids) > 0) {
          filtered_strategies <- values$strategies
          filtered_strategies$calls <- filtered_strategies$calls[rownames(filtered_strategies$calls) %in% track_ids, , drop = FALSE]
          values$filtered_strategies <- filtered_strategies
        }
      }
      
      # Notify user
      showNotification("Filtros aplicados correctamente", type = "message")
    })
    
    # Clear filters button
    observeEvent(input$clear_filters, {
      # Reset all filters
      updateSelectInput(session, "probe_filter", selected = "all")
      
      if (!is.null(input$arena_filter)) {
        updateSelectInput(session, "arena_filter", selected = character(0))
      }
      
      if (!is.null(input$day_filter)) {
        updateSelectInput(session, "day_filter", selected = numeric(0))
      }
      
      if (!is.null(input$target_id_filter)) {
        updateSelectInput(session, "target_id_filter", selected = character(0))
      }
      
      if (!is.null(input$custom1_values)) {
        updateSelectInput(session, "custom1_values", selected = character(0))
      }
      
      if (!is.null(input$custom2_values)) {
        updateSelectInput(session, "custom2_values", selected = character(0))
      }
      
      # Reset filter state
      filters_state(list(
        probe = "all",
        arenas = character(0),
        days = numeric(0),
        target_ids = character(0),
        custom1 = list(field = NULL, values = NULL),
        custom2 = list(field = NULL, values = NULL)
      ))
      
      # Reset global filter values
      values$active_filters <- NULL
      values$filtered_data <- values$processed_data
      values$filtered_strategies <- values$strategies
      
      showNotification("Filtros limpiados", type = "message")
    })
    
    # Display active filters
    output$filter_status <- renderUI({
      filters <- filters_state()
      
      filter_text <- c()
      
      if (!is.null(filters$probe) && filters$probe != "all") {
        filter_text <- c(filter_text, paste("Probe:", 
                                         if(filters$probe == "true") "TRUE" else "FALSE"))
      }
      
      if (!is.null(filters$arenas) && length(filters$arenas) > 0) {
        filter_text <- c(filter_text, paste("Arena:", paste(filters$arenas, collapse=", ")))
      }
      
      if (!is.null(filters$days) && length(filters$days) > 0) {
        filter_text <- c(filter_text, paste("Día:", paste(filters$days, collapse=", ")))
      }
      
      if (!is.null(filters$target_ids) && length(filters$target_ids) > 0) {
        filter_text <- c(filter_text, paste("Sujeto:", paste(filters$target_ids, collapse=", ")))
      }
      
      # Add custom filters if active
      if (!is.null(filters$custom1$field) && !is.null(filters$custom1$values) && 
          length(filters$custom1$values) > 0) {
        filter_text <- c(filter_text, paste(filters$custom1$field, ":", 
                                         paste(filters$custom1$values, collapse=", ")))
      }
      
      if (!is.null(filters$custom2$field) && !is.null(filters$custom2$values) && 
          length(filters$custom2$values) > 0) {
        filter_text <- c(filter_text, paste(filters$custom2$field, ":", 
                                         paste(filters$custom2$values, collapse=", ")))
      }
      
      if (length(filter_text) > 0) {
        tags$div(
          class = "alert alert-success",
          icon("filter"), 
          strong("Filtros activos: "), 
          HTML(paste(filter_text, collapse="<br>"))
        )
      } else {
        tags$div(
          class = "alert alert-info",
          icon("info-circle"),
          "No hay filtros activos. Mostrando todos los datos."
        )
      }
    })
    
    # Return reactive value with current filter state for use by parent module
    return(filters_state)
  })
}

# Helper function to apply filters to data
apply_filters <- function(data, filters) {
  if (is.null(data) || is.null(data$factors) || nrow(data$factors) == 0) {
    return(NULL)
  }
  
  # Make a copy to avoid modifying original
  result <- data
  filtered_factors <- data$factors
  
  # Apply Probe filter
  if (!is.null(filters$probe) && filters$probe != "all" && "Probe" %in% names(filtered_factors)) {
    probe_value <- filters$probe == "true"
    
    # Handle different probe column types (logical, character, etc)
    if (is.character(filtered_factors$Probe)) {
      # Handle case-insensitive TRUE/FALSE strings
      is_true <- grepl("^true$|^t$|^yes$|^y$|^1$", filtered_factors$Probe, ignore.case = TRUE)
      
      if (probe_value) {
        filtered_factors <- filtered_factors[is_true, , drop = FALSE]
      } else {
        filtered_factors <- filtered_factors[!is_true, , drop = FALSE]
      }
    } else {
      # For logical or numeric (0/1) values
      filtered_factors <- filtered_factors[filtered_factors$Probe == probe_value, , drop = FALSE]
    }
  }
  
  # Apply Arena filter
  if (!is.null(filters$arenas) && length(filters$arenas) > 0 && filters$arenas[1] != "" && 
      "_Arena" %in% names(filtered_factors)) {
    # Handle both with and without .txt extension
    arenas_with_txt <- unique(c(filters$arenas, paste0(filters$arenas, ".txt")))
    arenas_without_txt <- unique(gsub("\\.txt$", "", arenas_with_txt))
    all_arena_variants <- unique(c(arenas_with_txt, arenas_without_txt))
    
    filtered_factors <- filtered_factors[filtered_factors$`_Arena` %in% all_arena_variants, , drop = FALSE]
  }
  
  # Apply Day filter
  if (!is.null(filters$days) && length(filters$days) > 0 && "_Day" %in% names(filtered_factors)) {
    filtered_factors <- filtered_factors[filtered_factors$`_Day` %in% filters$days, , drop = FALSE]
  }
  
  # Apply Target ID filter
  if (!is.null(filters$target_ids) && length(filters$target_ids) > 0 && filters$target_ids[1] != "" && 
      "_TargetID" %in% names(filtered_factors)) {
    filtered_factors <- filtered_factors[filtered_factors$`_TargetID` %in% filters$target_ids, , drop = FALSE]
  }
  
  # Apply custom filter 1
  if (!is.null(filters$custom1$field) && !is.null(filters$custom1$values) && 
      length(filters$custom1$values) > 0 && filters$custom1$field %in% names(filtered_factors)) {
    filtered_factors <- filtered_factors[filtered_factors[[filters$custom1$field]] %in% filters$custom1$values, , drop = FALSE]
  }
  
  # Apply custom filter 2
  if (!is.null(filters$custom2$field) && !is.null(filters$custom2$values) && 
      length(filters$custom2$values) > 0 && filters$custom2$field %in% names(filtered_factors)) {
    filtered_factors <- filtered_factors[filtered_factors[[filters$custom2$field]] %in% filters$custom2$values, , drop = FALSE]
  }
  
  # Check if any data remains after filtering
  if (nrow(filtered_factors) == 0) {
    return(NULL) # Return NULL if no data matches filters
  }
  
  # Track IDs remaining after filtering
  remaining_ids <- unique(filtered_factors$`_TrackID`)
  
  # Update factors in result
  result$factors <- filtered_factors
  
  # Filter metrics if present (for density maps)
  if (!is.null(result$metrics) && is.list(result$metrics)) {
    if (!is.null(names(result$metrics))) {
      # If metrics are named by track ID
      result$metrics <- result$metrics[names(result$metrics) %in% remaining_ids]
    } else {
      # Filter based on track ID (assuming metrics list corresponds to factors order)
      track_ids_order <- result$factors$`_TrackID`
      valid_indices <- which(track_ids_order %in% remaining_ids)
      result$metrics <- result$metrics[valid_indices]
    }
  }
  
  # Filter paths if present
  if (!is.null(result$paths) && is.list(result$paths)) {
    if (!is.null(names(result$paths))) {
      # If paths are named by track ID
      result$paths <- result$paths[names(result$paths) %in% remaining_ids]
    } else {
      # Filter based on track ID order
      track_ids_order <- result$factors$`_TrackID`
      valid_indices <- which(track_ids_order %in% remaining_ids)
      result$paths <- result$paths[valid_indices]
    }
  }
  
  return(result)
}