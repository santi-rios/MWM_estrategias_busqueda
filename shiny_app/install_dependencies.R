# ==============================================================================
# INSTALACIÓN DE DEPENDENCIAS PARA MWM ANÁLISIS CON PAK
# ==============================================================================
# Script para instalar todas las dependencias necesarias usando 'pak'
# Pak es un gestor de paquetes más robusto que maneja mejor las dependencias
# ==============================================================================

cat("🎯 Instalando dependencias para MWM Analysis Tool con pak...\n\n")

# Función para instalar pak si no está disponible
install_pak_if_needed <- function() {
  if (!requireNamespace("pak", quietly = TRUE)) {
    cat("📦 Instalando pak para mejor gestión de dependencias...\n")
    install.packages("pak", repos = "https://cloud.r-project.org")
    if (!requireNamespace("pak", quietly = TRUE)) {
      stop("❌ No se pudo instalar pak. Instálalo manualmente con: install.packages('pak')")
    }
  }
  cat("✅ pak está disponible\n\n")
}

# Función segura para instalar paquetes con pak
safe_install_pak <- function(packages, category = "paquetes") {
  cat(sprintf("📦 Instalando %s %s...\n", length(packages), category))
  
  for (pkg in packages) {
    cat(sprintf("  • %s... ", pkg))
    tryCatch({
      pak::pak(pkg)
      cat("✅\n")
    }, error = function(e) {
      cat(sprintf("❌ Error: %s\n", e$message))
    })
  }
  cat("\n")
}

# INICIO DE INSTALACIÓN
cat("🚀 INICIO DE INSTALACIÓN\n")
cat("========================\n\n")

# 1. Instalar pak primero
install_pak_if_needed()

# 2. PAQUETES CRÍTICOS BÁSICOS
critical_packages <- c(
  "dplyr",      # Manipulación de datos
  "ggplot2",    # Gráficos
  "readxl"      # Leer archivos Excel
)

cat("🔧 PASO 1: Instalando paquetes críticos básicos...\n")
safe_install_pak(critical_packages, "paquetes críticos")

# 3. INSTALACIÓN ESPECIAL DE RTRACK
cat("🔬 PASO 2: Instalando rtrack (paquete especializado)...\n")
cat("  • rtrack... ")
rtrack_success <- FALSE
tryCatch({
  # Intentar pak primero con GitHub
  pak::pak("rupertoverall/Rtrack")
  rtrack_success <- TRUE
  cat("✅ (via pak)\n")
}, error = function(e) {
  # Si pak falla, usar devtools como respaldo
  cat("⚠️ pak falló, intentando devtools...\n")
  tryCatch({
    if (!requireNamespace("devtools", quietly = TRUE)) {
      pak::pak("devtools")
    }
    devtools::install_github("rupertoverall/Rtrack", upgrade = "never")
    rtrack_success <- TRUE
    cat("  • rtrack... ✅ (via devtools)\n")
  }, error = function(e2) {
    cat("  • rtrack... ❌ Error: ", e2$message, "\n")
    cat("    💡 Instala manualmente: devtools::install_github('rupertoverall/Rtrack')\n")
  })
})

# 4. PAQUETES PARA APLICACIÓN WEB
shiny_packages <- c(
  "shiny",
  "shinydashboard", 
  "shinyWidgets",
  "DT"
)

cat("\n🌐 PASO 3: Instalando paquetes de Shiny...\n")
safe_install_pak(shiny_packages, "paquetes de Shiny")

# 5. PAQUETES DE VISUALIZACIÓN AVANZADA
visualization_packages <- c(
  "viridis",    # Paletas de colores
  "plotly",     # Gráficos interactivos
  "scales"      # Formateo de escalas
)

cat("📊 PASO 4: Instalando paquetes de visualización...\n")
safe_install_pak(visualization_packages, "paquetes de visualización")

# 6. PAQUETES ESTADÍSTICOS AVANZADOS
stats_packages <- c(
  "glmmTMB",    # Modelos mixtos avanzados
  "emmeans"     # Comparaciones post-hoc
)

cat("📈 PASO 5: Instalando paquetes estadísticos...\n")
safe_install_pak(stats_packages, "paquetes estadísticos")

# 7. PAQUETES UTILITARIOS
utility_packages <- c(
  "writexl",    # Escribir Excel
  "forcats",    # Manejo de factores
  "zip"         # Comprimir archivos
)

cat("🛠️ PASO 6: Instalando paquetes utilitarios...\n")
safe_install_pak(utility_packages, "paquetes utilitarios")

# VERIFICACIÓN FINAL
cat("🔍 VERIFICACIÓN FINAL\n")
cat("=====================\n")

all_packages <- c(critical_packages, shiny_packages, visualization_packages, stats_packages, utility_packages)
missing <- c()

for (pkg in all_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    missing <- c(missing, pkg)
  }
}

# Verificar rtrack especialmente
rtrack_available <- requireNamespace("Rtrack", quietly = TRUE)

if (length(missing) == 0 && rtrack_available) {
  cat("✅ ¡Todas las dependencias instaladas correctamente con pak!\n")
  cat("🎉 Puedes proceder a usar el análisis MWM\n\n")
  cat("📝 Para empezar:\n")
  cat("   • Para aplicación web: source('shiny_app/run_app.R')\n")
  cat("   • Para análisis directo: source('simple_analysis.R')\n")
  cat("   • Para guía interactiva: source('start_here.R')\n")
} else {
  cat("⚠️ Instalación parcialmente completada:\n")
  if (length(missing) > 0) {
    cat("   Paquetes faltantes:\n")
    for (pkg in missing) {
      cat(sprintf("   • %s\n", pkg))
    }
  }
  if (!rtrack_available) {
    cat("   • rtrack (CRÍTICO) - no disponible\n")
  }
  cat("\n💡 Para paquetes faltantes: pak::pak(c(", paste0("'", missing, "'", collapse = ", "), "))\n")
  if (!rtrack_available) {
    cat("💡 Para rtrack: devtools::install_github('rupertoverall/Rtrack')\n")
  }
}

# VERIFICACIÓN DE FUNCIONALIDAD BÁSICA
cat("\n🧪 VERIFICACIÓN DE FUNCIONALIDAD\n")
cat("=================================\n")

# Test rtrack
cat("• rtrack: ")
if (rtrack_available) {
  cat("✅ disponible\n")
} else {
  cat("❌ no disponible\n")
}

# Test análisis básico
cat("• Análisis básico: ")
if (requireNamespace("dplyr", quietly = TRUE) && 
    requireNamespace("ggplot2", quietly = TRUE) && 
    requireNamespace("readxl", quietly = TRUE)) {
  cat("✅ funcional\n")
} else {
  cat("❌ faltan dependencias\n")
}

# Test aplicación web
cat("• Aplicación web: ")
if (requireNamespace("shiny", quietly = TRUE) && 
    requireNamespace("shinydashboard", quietly = TRUE)) {
  cat("✅ disponible\n")
} else {
  cat("⚠️ limitada\n")
}

cat("\n" , rep("=", 50), "\n")
cat("✨ Instalación con pak completada\n")
