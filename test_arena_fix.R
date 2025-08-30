# Script de prueba para verificar la corrección del error de arena
# Simula el procesamiento de un experimento con la nueva lógica

library(Rtrack)
library(readxl)
library(writexl)

# Cargar funciones de utilidad
source("shiny_app/utils/processing_functions.R")

# Leer datos de prueba
experiment_data <- read_excel("data/test_dataset/MWM_Experiment_File.xlsx")

# Configuración de arena simulada (valores típicos)
arena_config <- list(
  center_x = 50,
  center_y = 50, 
  radius = 45,
  goal_x = 27.5,
  goal_y = 27.5,
  goal_radius = 5
)

cat("🧪 Probando corrección de arena en MWM...\n\n")

cat("1. Datos del experimento:\n")
cat("   Tracks:", nrow(experiment_data), "\n")
cat("   Arenas únicas:", length(unique(experiment_data$`_Arena`)), "\n")
cat("   Arena principal:", unique(experiment_data$`_Arena`)[1], "\n\n")

cat("2. Creando archivo temporal de experimento...\n")
temp_exp_file <- tempfile(fileext = ".xlsx")
writexl::write_xlsx(experiment_data, temp_exp_file)
cat("   ✓ Archivo creado:", basename(temp_exp_file), "\n\n")

cat("3. Creando archivo de arena...\n")
arena_name <- unique(experiment_data$`_Arena`)[1]
arena_file <- create_arena_file(
  arena_info = arena_config,
  arena_name = arena_name,
  output_dir = dirname(temp_exp_file)
)
cat("   ✓ Arena creada:", basename(arena_file), "\n")
cat("   Contenido:\n")
arena_content <- readLines(arena_file)
for (line in arena_content) {
  cat("   ", line, "\n")
}

cat("\n4. Probando lectura de experimento con Rtrack...\n")
tryCatch({
  experiment <- process_mwm_experiment(
    experiment_file = temp_exp_file,
    data_dir = "data/test_dataset",
    project_dir = dirname(temp_exp_file),
    threads = 1
  )
  
  cat("   ✅ Experimento procesado exitosamente!\n")
  cat("   Tracks leídos:", length(experiment$tracks), "\n")
  cat("   Arena configurada:", experiment$arena$type, "\n")
  
}, error = function(e) {
  cat("   ❌ Error:", e$message, "\n")
})

# Limpiar archivos temporales
unlink(temp_exp_file)
unlink(arena_file)

cat("\n🎯 Prueba completada!\n")
