# 🎯 Morris Water Maze Analysis Tool

Una aplicación Shiny para el análisis automatizado de experimentos del Laberinto Acuático de Morris (MWM) utilizando el paquete Rtrack.

## 🚀 Características

- **Interfaz intuitiva**: Flujo de trabajo paso a paso
- **Carga de datos simplificada**: Soporte para múltiples formatos
- **Configuración automática de arena**: Con vista previa visual
- **Procesamiento paralelo**: Para experimentos grandes
- **Mapas de densidad**: Visualización de patrones de navegación
- **Análisis de estrategias**: Clasificación automática y análisis estadístico
- **Exportación completa**: Descarga de todos los resultados

## 📋 Requisitos

### Paquetes de R necesarios:

```r
# Instalar paquetes necesarios
install.packages(c(
  "shiny", "shinydashboard", "shinyWidgets",
  "DT", "plotly", "viridis", "dplyr", "ggplot2", 
  "ggprism", "forcats", "readxl", "writexl", "scales"
))

# Instalar Rtrack (si no está instalado)
if (!require(Rtrack)) {
  install.packages("devtools")
  devtools::install_github("rupertoverall/Rtrack")
}
```

## 🏃‍♂️ Inicio Rápido

### 1. Ejecutar la aplicación

```r
# Navegar al directorio de la aplicación
setwd("path/to/MWM_estrategias_busqueda/shiny_app")

# Ejecutar la aplicación
shiny::runApp()
```

### 2. Flujo de trabajo

1. **📁 Cargar Datos**
   - Sube tu archivo Excel con la descripción del experimento
   - Carga todos los archivos de tracks (.csv o .txt)
   - Verifica que todos los archivos estén presentes

2. **🎯 Configurar Arena**
   - Define las dimensiones del pool circular
   - Configura las posiciones de objetivos
   - Genera automáticamente los archivos de arena

3. **⚙️ Procesar Análisis**
   - Configura parámetros de procesamiento
   - Define variables de agrupación
   - Ejecuta el análisis automático con Rtrack

4. **📊 Ver Resultados**
   - Explora mapas de densidad por grupos
   - Analiza estrategias de búsqueda
   - Revisa análisis estadísticos

5. **💾 Exportar**
   - Descarga todos los resultados en formato ZIP
   - Obtén gráficos en alta resolución
   - Accede a datos procesados en CSV

## 📁 Estructura de Archivos

### Archivo de Experimento (Excel)
El archivo debe contener las siguientes columnas **obligatorias**:

| Columna | Descripción |
|---------|-------------|
| `_TrackID` | Identificador único del track |
| `_TargetID` | Identificador del sujeto |
| `_Day` | Día del experimento |
| `_Trial` | Número de trial |
| `_Arena` | Archivo de arena correspondiente |
| `_TrackFile` | Archivo de coordenadas |
| `_TrackFileFormat` | Formato del archivo (ej: ethovision.3.csv) |

**Columnas adicionales** (opcionales para agrupación):
- `Tratamiento`: Tipo de tratamiento
- `Estres`: Condición de estrés
- Cualquier otra variable experimental

### Archivos de Tracks
Formatos soportados:
- **CSV**: Con columnas Time, X, Y
- **Ethovision**: Formatos estándar de Ethovision
- **Raw formats**: raw.csv, raw.tab, raw.csv2

## 🎯 Análisis de Estrategias

La aplicación clasifica automáticamente las estrategias en:

### 🧠 **Alocéntricas** (Hipocampo-dependientes)
- Directed search
- Corrected path  
- Direct path

### 🔄 **Egocéntricas** (Hipocampo-independientes)
- Thigmotaxis
- Circling
- Random path
- Scanning
- Chaining

### ⚡ **Otras**
- Perseverance

## 🗺️ Mapas de Densidad

- **Múltiples grupos**: Visualización comparativa
- **Paletas personalizables**: Viridis, Plasma, Inferno, etc.
- **Alta resolución**: Hasta 2000x2000 píxeles
- **Configuración flexible**: Niveles de color ajustables

## 📊 Análisis Estadístico

- **Modelos mixtos**: Para datos longitudinales
- **Tests Chi-cuadrado**: Para diferencias entre grupos
- **Métricas de confianza**: Evaluación de calidad de clasificación
- **Tablas resumen**: Por grupo y tiempo

## 🔧 Configuración Avanzada

### Procesamiento Paralelo
- Automático: `threads = 0`
- Manual: Especifica número de hilos
- Recomendado para experimentos > 100 tracks

### Umbral de Confianza
- Por defecto: Sin umbral
- Recomendado: 0.4 para mayor precisión
- Filtra estrategias con baja confianza

## 📈 Outputs

### Gráficos
- **Mapas de densidad**: PNG, 300 DPI
- **Análisis de estrategias**: Barras apiladas, líneas temporales
- **Formato**: ggplot2, alta calidad

### Datos
- **Experimento procesado**: Métricas completas de Rtrack
- **Estrategias clasificadas**: Con confianza y categorías
- **Datos longitudinales**: Para análisis estadístico
- **Formato**: CSV, compatible con R/SPSS/Excel

## 🐛 Solución de Problemas

### Error: "Faltan columnas requeridas"
- Verifica que tu archivo Excel tenga todas las columnas obligatorias
- Los nombres deben empezar con `_` para columnas del sistema

### Error: "Archivo de track no encontrado"
- Asegúrate de que los nombres en `_TrackFile` coincidan exactamente
- Verifica que todos los archivos estén cargados

### Error: "No se puede procesar track"
- Revisa el formato de tus archivos de coordenadas
- Verifica que tengan columnas Time, X, Y
- Prueba diferentes valores en `_TrackFileFormat`

### Rendimiento lento
- Activa procesamiento paralelo
- Reduce la resolución de mapas de densidad
- Filtra datos por días específicos

## 🤝 Contribuir

Este proyecto está basado en tu flujo de trabajo documentado en `ejemplo.qmd`. Para mejoras:

1. Modificar módulos en `/modules/`
2. Añadir funciones en `/utils/`
3. Actualizar estilos en `/www/custom.css`

## 📚 Referencias

- **Rtrack**: [rupertoverall/Rtrack](https://github.com/rupertoverall/Rtrack)
- **Morris Water Maze**: Metodología estándar para evaluación espacial
- **Shiny**: Framework para aplicaciones web en R

## 📄 Licencia

Proyecto desarrollado para análisis de experimentos MWM. Basado en el paquete Rtrack y metodologías estándar del campo.

---

*Desarrollado por Santiago Rios para facilitar el análisis de experimentos del Laberinto Acuático de Morris* 🧠🔬
