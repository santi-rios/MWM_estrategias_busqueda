# Script de prueba para verificar la generación de archivos de arena
# Simula exactamente la estructura del módulo de arena

# Cargar funciones de utilidad
source("shiny_app/utils/processing_functions.R")

# Configuración de arena simulada con la estructura correcta del módulo
arena_config_test <- list(
  type = "mwm",
  time_units = "s",
  arena_bounds = list(
    shape = "circle",
    center_x = 133.655,
    center_y = 103.5381,
    radius = 95
  ),
  goal = list(
    shape = "circle",
    center_x = 121.8934,
    center_y = 154.6834,
    radius = 10,
    quadrant = "SW"
  )
)

cat("🧪 Probando generación de archivo de arena con estructura correcta...\n\n")

# Crear directorio temporal
temp_dir <- tempfile("arena_test_")
dir.create(temp_dir)

# Crear archivo de arena
arena_file <- create_arena_file(
  arena_info = arena_config_test,
  arena_name = "Arena_SW",
  output_dir = temp_dir
)

cat("✅ Archivo de arena creado:", arena_file, "\n\n")

# Mostrar contenido
cat("📄 Contenido del archivo:\n")
arena_content <- readLines(arena_file)
for (i in seq_along(arena_content)) {
  cat(sprintf("%2d: %s\n", i, arena_content[i]))
}

cat("\n🔍 Probando lectura con Rtrack...\n")
tryCatch({
  library(Rtrack)
  arena <- Rtrack::read_arena(arena_file)
  cat("   ✅ Arena leída exitosamente!\n")
  cat("   Tipo:", arena$type, "\n")
}, error = function(e) {
  cat("   ❌ Error:", e$message, "\n")
})

# Limpiar
unlink(temp_dir, recursive = TRUE)

cat("\n🎯 Prueba completada!\n")
