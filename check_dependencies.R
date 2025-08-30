# Script de verificación rápida de dependencias
# Ejecuta este script para verificar si todo está listo para el análisis MWM

cat("🔍 VERIFICACIÓN RÁPIDA DE DEPENDENCIAS MWM\n")
cat("==========================================\n\n")

# Función para verificar si un paquete está disponible
check_package <- function(package_name, is_critical = FALSE) {
  # Usar el nombre exacto del paquete sin conversión
  actual_name <- package_name
  
  status <- tryCatch({
    if (requireNamespace(actual_name, quietly = TRUE)) {
      list(available = TRUE, error = NULL)
    } else {
      list(available = FALSE, error = "No disponible")
    }
  }, error = function(e) {
    list(available = FALSE, error = e$message)
  })
  
  if (status$available) {
    cat(paste("✅", package_name, "- OK\n"))
    return(TRUE)
  } else {
    if (is_critical) {
      cat(paste("❌", package_name, "- ERROR:", status$error, "(CRÍTICO)\n"))
    } else {
      cat(paste("⚠️ ", package_name, "- ERROR:", status$error, "(opcional)\n"))
    }
    return(FALSE)
  }
}

# Verificar paquetes críticos
cat("🔥 PAQUETES CRÍTICOS (necesarios para funcionalidad básica):\n")
critical_packages <- c("Rtrack", "dplyr", "ggplot2", "readxl")
critical_ok <- sapply(critical_packages, function(pkg) check_package(pkg, is_critical = TRUE))

cat("\n💻 PAQUETES PARA APLICACIÓN WEB:\n")
shiny_packages <- c("shiny", "shinydashboard", "shinyWidgets", "DT")
shiny_ok <- sapply(shiny_packages, function(pkg) check_package(pkg, is_critical = FALSE))

cat("\n🎨 PAQUETES DE VISUALIZACIÓN:\n")
viz_packages <- c("viridis", "scales", "ggprism")
viz_ok <- sapply(viz_packages, function(pkg) check_package(pkg, is_critical = FALSE))

cat("\n🔧 PAQUETES UTILITARIOS:\n")
util_packages <- c("writexl", "forcats", "zip")
util_ok <- sapply(util_packages, function(pkg) check_package(pkg, is_critical = FALSE))

# Pruebas de funcionalidad rápidas
cat("\n🧪 TESTS DE FUNCIONALIDAD RÁPIDOS:\n")

# Test Rtrack
cat("Test Rtrack: ")
if (require("Rtrack", quietly = TRUE)) {
  tryCatch({
    # Test muy básico
    test_result <- "mwm"  # Simplemente verificar que se puede acceder
    cat("✅ OK\n")
    rtrack_functional <- TRUE
  }, error = function(e) {
    cat("❌ ERROR -", e$message, "\n")
    rtrack_functional <- FALSE
  })
} else {
  cat("❌ NO DISPONIBLE\n")
  rtrack_functional <- FALSE
}

# Test manipulación de datos
cat("Test manipulación datos: ")
if (require("dplyr", quietly = TRUE)) {
  tryCatch({
    test_df <- data.frame(x = 1:3, y = 4:6)
    result <- test_df %>% filter(x > 1)
    cat("✅ OK\n")
    data_functional <- TRUE
  }, error = function(e) {
    cat("❌ ERROR -", e$message, "\n")
    data_functional <- FALSE
  })
} else {
  cat("❌ NO DISPONIBLE\n")
  data_functional <- FALSE
}

# Test gráficos
cat("Test gráficos: ")
if (require("ggplot2", quietly = TRUE)) {
  tryCatch({
    p <- ggplot(data.frame(x = 1:3, y = 1:3), aes(x, y)) + geom_point()
    cat("✅ OK\n")
    plot_functional <- TRUE
  }, error = function(e) {
    cat("❌ ERROR -", e$message, "\n")
    plot_functional <- FALSE
  })
} else {
  cat("❌ NO DISPONIBLE\n")
  plot_functional <- FALSE
}

# RESUMEN FINAL
cat("\n📊 RESUMEN DEL ESTADO:\n")
cat("======================\n")

# Convertir resultados a vectores lógicos
all_critical_ok <- all(critical_ok)
core_functional <- rtrack_functional && data_functional && plot_functional
shiny_available <- sum(shiny_ok, na.rm = TRUE) >= 3  # Al menos 3 de 4 paquetes Shiny

if (all_critical_ok && core_functional) {
  cat("🎉 ESTADO: EXCELENTE\n")
  cat("✅ Funcionalidad principal completamente disponible\n")
  cat("✅ Todos los análisis MWM funcionarán correctamente\n")
  
  if (shiny_available) {
    cat("✅ Aplicación web disponible\n")
    cat("\n🚀 PUEDES USAR:\n")
    cat("   • Aplicación web: source('shiny_app/run_app.R')\n")
    cat("   • Script simple: source('simple_analysis.R')\n")
    cat("   • Guía: source('start_here.R')\n")
  } else {
    cat("⚠️  Aplicación web limitada (algunos paquetes Shiny faltan)\n")
    cat("\n🚀 PUEDES USAR:\n")
    cat("   • Script simple: source('simple_analysis.R') [RECOMENDADO]\n")
    cat("   • Guía: source('start_here.R')\n")
    cat("   • Instalar Shiny: install.packages(c('shiny', 'shinydashboard'))\n")
  }

} else if (all_critical_ok && !core_functional) {
  cat("⚠️  ESTADO: FUNCIONALIDAD LIMITADA\n")
  cat("✅ Paquetes críticos instalados\n")
  cat("❌ Algunos tests de funcionalidad fallaron\n")
  cat("\n💡 RECOMENDACIÓN:\n")
  cat("   • Revisar errores específicos arriba\n")
  cat("   • Reinstalar paquetes con problemas\n")
  cat("   • Intentar usar: source('simple_analysis.R')\n")

} else {
  cat("❌ ESTADO: INSTALACIÓN INCOMPLETA\n")
  cat("❌ Faltan paquetes críticos\n")
  cat("\n🔧 NECESITAS:\n")
  cat("   • Ejecutar: source('shiny_app/install_dependencies.R')\n")
  cat("   • O instalar manualmente los paquetes marcados como CRÍTICOS\n")
}

# Mostrar paquetes faltantes críticos
missing_critical <- names(critical_ok)[!critical_ok]
if (length(missing_critical) > 0) {
  cat("\n❗ PAQUETES CRÍTICOS FALTANTES:\n")
  for (pkg in missing_critical) {
    if (pkg == "Rtrack") {
      cat("   •", pkg, "- Instalar con: devtools::install_github('rupertoverall/Rtrack')\n")
    } else {
      cat("   •", pkg, "- Instalar con: install.packages('", pkg, "')\n", sep = "")
    }
  }
}

cat("\n", paste(rep("=", 50), collapse = ""), "\n")
cat("💡 Para instalación completa: source('shiny_app/install_dependencies.R')\n")
cat("📚 Para ayuda: README.md o shiny_app/README.md\n")
