# Test de generación de archivos de arena mejorados
# Verifica que se generen correctamente con old.goal

# Simular parámetros de entrada
center_x <- 133.655
center_y <- 103.5381
arena_radius <- 95
goal_radius <- 10

# Arena SW
arena_name <- "Arena_SW"
goal_x <- 121.8934
goal_y <- 154.6834

# Para experimentos de reversión, necesitamos tanto goal como old.goal
old_goal_x <- ifelse(grepl("SW", arena_name), 140.42685, 121.8934)
old_goal_y <- ifelse(grepl("SW", arena_name), 51.32352, 154.6834)

arena_content_sw <- paste(
  "type = mwm",
  "time.units = s",
  "trial.length = 120",
  "",
  "# For circular pool/goal, give values in this order:",
  "# shape centre.x centre.y radius",
  "",
  paste("arena.bounds = circle", center_x, center_y, arena_radius),
  "",
  paste("goal = circle", goal_x, goal_y, goal_radius),
  "",
  paste("old.goal = circle", old_goal_x, old_goal_y, goal_radius),
  "",
  sep = "\n"
)

# Arena NE
arena_name <- "Arena_NE"
goal_x <- 140.42685
goal_y <- 51.32352

# Para experimentos de reversión, necesitamos tanto goal como old.goal
old_goal_x <- ifelse(grepl("SW", arena_name), 140.42685, 121.8934)
old_goal_y <- ifelse(grepl("SW", arena_name), 51.32352, 154.6834)

arena_content_ne <- paste(
  "type = mwm",
  "time.units = s",
  "trial.length = 120",
  "",
  "# For circular pool/goal, give values in this order:",
  "# shape centre.x centre.y radius",
  "",
  paste("arena.bounds = circle", center_x, center_y, arena_radius),
  "",
  paste("goal = circle", goal_x, goal_y, goal_radius),
  "",
  paste("old.goal = circle", old_goal_x, old_goal_y, goal_radius),
  "",
  sep = "\n"
)

# Mostrar resultados
cat("=== Arena SW ===\n")
cat(arena_content_sw)
cat("\n\n=== Arena NE ===\n")
cat(arena_content_ne)

# Guardar archivos de prueba
writeLines(arena_content_sw, "Arena_SW_test.txt")
writeLines(arena_content_ne, "Arena_NE_test.txt")

cat("\n\n✅ Archivos de prueba generados: Arena_SW_test.txt y Arena_NE_test.txt")
