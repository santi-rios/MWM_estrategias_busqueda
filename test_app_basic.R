# Script básico para probar la aplicación MWM
# Este script verifica que la aplicación se ejecute correctamente

# Verificar que todos los archivos necesarios existen
check_files <- function() {
  required_files <- c(
    "shiny_app/app.R",
    "shiny_app/modules/data_input_module.R",
    "shiny_app/modules/arena_config_module.R", 
    "shiny_app/modules/analysis_module.R",
    "shiny_app/modules/results_module.R",
    "data/test_dataset/MWM_Experiment_File.xlsx"
  )
  
  missing_files <- c()
  for (file in required_files) {
    if (!file.exists(file)) {
      missing_files <- c(missing_files, file)
    }
  }
  
  if (length(missing_files) > 0) {
    cat("❌ Archivos faltantes:\n")
    for (file in missing_files) {
      cat("  -", file, "\n")
    }
    return(FALSE)
  } else {
    cat("✅ Todos los archivos necesarios están presentes\n")
    return(TRUE)
  }
}

# Verificar dependencias
check_dependencies <- function() {
  required_packages <- c(
    "shiny", "shinydashboard", "shinyWidgets", "DT",
    "plotly", "Rtrack", "readxl", "writexl", "pak"
  )
  
  missing_packages <- c()
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing_packages <- c(missing_packages, pkg)
    }
  }
  
  if (length(missing_packages) > 0) {
    cat("❌ Paquetes faltantes:\n")
    for (pkg in missing_packages) {
      cat("  -", pkg, "\n")
    }
    cat("\nPara instalar usa: pak::pak(c('", paste(missing_packages, collapse = "', '"), "'))\n")
    return(FALSE)
  } else {
    cat("✅ Todas las dependencias están instaladas\n")
    return(TRUE)
  }
}

# Ejecutar verificaciones
cat("🔍 Verificando aplicación MWM...\n\n")

cat("1. Verificando archivos:\n")
files_ok <- check_files()

cat("\n2. Verificando dependencias:\n") 
deps_ok <- check_dependencies()

if (files_ok && deps_ok) {
  cat("\n✅ ¡La aplicación está lista para usar!\n")
  cat("Para ejecutarla:\n")
  cat("  1. cd shiny_app\n")
  cat("  2. R -e \"shiny::runApp()\"\n")
  cat("  3. Abre http://localhost:3737 en tu navegador\n\n")
  cat("📁 Archivos de prueba disponibles en: data/test_dataset/\n")
} else {
  cat("\n❌ La aplicación necesita correcciones antes de funcionar\n")
}
