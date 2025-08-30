# GUÍA RÁPIDA - MWM Analysis Tool
# ================================

# Esta guía te ayudará a elegir la mejor opción para tu flujo de trabajo

cat("🎯 MWM Analysis Tool - Guía de Inicio\n")
cat("====================================\n\n")

cat("Tienes 2 opciones para analizar tus datos de MWM:\n\n")

cat("1️⃣  APLICACIÓN WEB (Recomendada para principiantes)\n")
cat("   ✅ Interfaz gráfica intuitiva\n")
cat("   ✅ Paso a paso guiado\n")
cat("   ✅ No requiere conocimiento de R\n")
cat("   ✅ Vista previa de datos en tiempo real\n")
cat("   ✅ Validación automática de archivos\n")
cat("   \n")
cat("   Para usar: source('shiny_app/run_app.R')\n\n")

cat("2️⃣  SCRIPT SIMPLIFICADO (Para usuarios avanzados)\n")
cat("   ✅ Ejecución directa y rápida\n")
cat("   ✅ Totalmente personalizable\n")
cat("   ✅ Ideal para análisis repetitivos\n")
cat("   ✅ No requiere interfaz gráfica\n")
cat("   ✅ Fácil integración con otros scripts\n")
cat("   \n")
cat("   Para usar: source('simple_analysis.R')\n\n")

cat("🔧 VERIFICACIÓN E INSTALACIÓN (MEJORADO CON PAK)\n")
cat("===============================================\n")
cat("Antes de empezar, puedes:\n")
cat("3. Verificar dependencias: source('check_dependencies.R')\n")
cat("4. Instalar dependencias con pak: source('shiny_app/install_dependencies.R')\n")
cat("   💡 Ahora usando 'pak' para mejor gestión de dependencias\n\n")

cat("📁 DATOS DE EJEMPLO\n")
cat("==================\n")
cat("Incluidos en data/example/:\n")
cat("- Experiment_Example.csv: Archivo de experimento\n")
cat("- Track_1.csv: Ejemplo de archivo de coordenadas\n\n")

cat("🆘 AYUDA Y SOPORTE\n")
cat("==================\n")
cat("- README.md: Documentación completa\n")
cat("- shiny_app/README.md: Documentación de la app web\n")
cat("- ejemplo.qmd: Metodología original y ejemplos\n\n")

cat("💡 RECOMENDACIÓN:\n")
cat("Si es tu primera vez, usa la aplicación web.\n")
cat("Si ya conoces el flujo, el script simplificado es más rápido.\n\n")

# Función para detectar qué opción usar
detect_user_preference <- function() {
  cat("🤔 ¿Qué quieres hacer?\n")
  cat("1. Aplicación web (interfaz gráfica)\n")
  cat("2. Script simplificado (solo código)\n")
  cat("3. Verificar dependencias\n")
  cat("4. Instalar dependencias\n")
  cat("5. Ver ayuda\n\n")
  
  choice <- readline("Ingresa tu opción (1-5): ")
  
  switch(choice,
    "1" = {
      cat("🚀 Iniciando aplicación web...\n")
      cat("💡 Si hay errores, ejecuta primero la opción 3 para verificar dependencias\n")
      source("shiny_app/run_app.R")
    },
    "2" = {
      cat("⚙️ Ejecutando script simplificado...\n")
      cat("💡 Tip: Modifica las configuraciones al inicio del archivo\n")
      cat("💡 Si hay errores, ejecuta primero la opción 3 para verificar dependencias\n")
      source("simple_analysis.R")
    },
    "3" = {
      cat("🔍 Verificando estado de dependencias...\n")
      source("check_dependencies.R")
    },
    "4" = {
      cat("📦 Instalando dependencias...\n")
      source("shiny_app/install_dependencies.R")
    },
    "5" = {
      cat("📚 Abriendo documentación...\n")
      if (file.exists("README.md")) {
        file.show("README.md")
      }
      if (file.exists("ejemplo.qmd")) {
        file.show("ejemplo.qmd")
      }
    },
    {
      cat("❌ Opción no válida. Ejecuta este script nuevamente.\n")
    }
  )
}

# Ejecutar si se llama interactivamente
if (interactive()) {
  detect_user_preference()
}
