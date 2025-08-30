# Script de lanzamiento para la aplicación MWM Analysis Tool
# Ejecuta este archivo para iniciar la aplicación

# Verificar directorio de trabajo
if (!file.exists("app.R")) {
  stop("❌ Ejecuta este script desde el directorio shiny_app/")
}

cat("🎯 Iniciando MWM Analysis Tool...\n\n")

# Verificar dependencias críticas
critical_packages <- c("shiny", "Rtrack", "dplyr", "ggplot2")
missing_packages <- c()

for (pkg in critical_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    missing_packages <- c(missing_packages, pkg)
  }
}

if (length(missing_packages) > 0) {
  cat("❌ Faltan paquetes críticos:", paste(missing_packages, collapse = ", "), "\n")
  cat("💡 Ejecuta 'source(\"install_dependencies.R\")' primero\n\n")
  
  response <- readline("¿Intentar instalar automáticamente? (y/n): ")
  if (tolower(response) %in% c("y", "yes", "s", "si")) {
    cat("📦 Instalando dependencias...\n")
    source("install_dependencies.R")
  } else {
    stop("Instalación cancelada")
  }
}

cat("✅ Todas las dependencias están disponibles\n\n")

# Configurar opciones de la aplicación
options(
  shiny.maxRequestSize = 100*1024^2,  # 100MB para archivos grandes
  shiny.host = "127.0.0.1",
  shiny.port = 3838
)

cat("🚀 Configuración:\n")
cat("   - Tamaño máximo de archivo: 100MB\n")
cat("   - Host: 127.0.0.1\n")
cat("   - Puerto: 3838\n\n")

# Mostrar información del sistema
cat("💻 Información del sistema:\n")
cat(paste("   - R version:", R.version.string, "\n"))
cat(paste("   - Plataforma:", R.version$platform, "\n"))
cat(paste("   - Directorio:", getwd(), "\n\n"))

# Verificar archivos de ejemplo
if (file.exists("../data/example/Experiment_Example.csv")) {
  cat("📁 Datos de ejemplo disponibles en ../data/example/\n\n")
}

cat("🌐 Abriendo aplicación en el navegador...\n")
cat("   URL: http://127.0.0.1:3838\n\n")

cat("💡 Para detener la aplicación, presiona Ctrl+C en la consola\n\n")

# Lanzar la aplicación
tryCatch({
  shiny::runApp(
    appDir = ".",
    host = "127.0.0.1",
    port = 3838,
    launch.browser = TRUE
  )
}, error = function(e) {
  cat("❌ Error lanzando la aplicación:", e$message, "\n")
  cat("💡 Intenta ejecutar manualmente: shiny::runApp()\n")
}, interrupt = function() {
  cat("\n👋 Aplicación detenida por el usuario\n")
})
