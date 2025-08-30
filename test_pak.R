# Prueba rápida de instalación con pak
cat("🧪 Prueba de pak...\n")

# Instalar pak si no está disponible
if (!requireNamespace("pak", quietly = TRUE)) {
  cat("📦 Instalando pak...\n")
  install.packages("pak")
}

# Verificar pak
if (requireNamespace("pak", quietly = TRUE)) {
  cat("✅ pak disponible\n")
  
  # Probar instalación de un paquete pequeño
  cat("🔧 Instalando scales con pak...\n")
  pak::pak("scales")
  
  # Verificar instalación
  if (requireNamespace("scales", quietly = TRUE)) {
    cat("✅ scales instalado correctamente con pak\n")
  } else {
    cat("❌ Error instalando scales\n")
  }
} else {
  cat("❌ pak no se pudo instalar\n")
}

cat("🏁 Prueba completada\n")
