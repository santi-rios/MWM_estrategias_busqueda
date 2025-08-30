# 🎯 MWM Estrategias de Búsqueda - Herramienta de Análisis Automatizado

Una herramienta completa para el análisis automatizado de experimentos del Laberinto Acuático de Morris (MWM) utilizando el paquete Rtrack. Incluye tanto una aplicación web interactiva como scripts simplificados para el análisis.

## 🚀 Características Principales

- **Interfaz web intuitiva** (Shiny App) para usuarios sin experiencia en R
- **Script simplificado** para usuarios avanzados que prefieren código directo
- **Procesamiento automático** con el paquete Rtrack
- **Mapas de densidad** personalizables por grupos
- **Análisis de estrategias** con clasificación automática
- **Exportación completa** de resultados en múltiples formatos
- **Gestión de dependencias mejorada** con pak para instalaciones más robustas

## 📁 Estructura del Proyecto

```
MWM_estrategias_busqueda/
├── shiny_app/                    # Aplicación web interactiva
│   ├── app.R                     # Aplicación principal
│   ├── modules/                  # Módulos de la interfaz
│   ├── utils/                    # Funciones auxiliares
│   ├── www/                      # Archivos CSS y recursos
│   ├── README.md                 # Documentación de la app
│   ├── install_dependencies.R    # Instalador con pak (MEJORADO)
│   └── run_app.R                 # Script de lanzamiento
├── simple_analysis.R             # Script simplificado (sin interfaz)
├── check_dependencies.R          # Verificador de dependencias (MEJORADO)
├── start_here.R                  # Guía interactiva de inicio
├── data/example/                 # Datos de ejemplo
├── ejemplo.qmd                   # Documentación metodológica
└── README.md                     # Este archivo
```

## 🏃‍♂️ Inicio Rápido

### Opción 1: Aplicación Web (Recomendada para principiantes)

1. **Instalar dependencias (MEJORADO CON PAK):**

```r
source("shiny_app/install_dependencies.R")  # Usa pak para mejor gestión
```

2. **Verificar instalación (OPCIONAL):**

```r
source("check_dependencies.R")  # Verifica que todo funcione
```

3. **Ejecutar la aplicación:**

```r
source("shiny_app/run_app.R")
```

4. **Seguir el flujo paso a paso:**
   - Cargar datos de experimento
   - Configurar arena
   - Procesar análisis
   - Visualizar resultados
   - Exportar todo

### Opción 2: Script Simplificado (Para usuarios avanzados)

1. **Modificar configuración** en `simple_analysis.R`:
```r
# Configurar rutas de tus datos
EXPERIMENT_FILE <- "ruta/a/tu/experimento.xlsx"
DATA_DIR <- "ruta/a/tus/tracks/"

# Configurar arena según tu setup
ARENA_CONFIG <- list(
  center_x = 133.655,
  center_y = 103.5381,
  radius = 95,
  # ... más parámetros
)
```

2. **Ejecutar análisis:**
```r
source("simple_analysis.R")
```

## 📊 Tipos de Análisis Incluidos

### 🗺️ Mapas de Densidad
- Visualización de patrones de navegación
- Comparación entre grupos experimentales
- Múltiples paletas de colores
- Alta resolución para publicaciones

### 🧠 Análisis de Estrategias
Las estrategias se clasifican automáticamente en:

- **Alocéntricas** (Hipocampo-dependientes):
  - Directed search, Corrected path, Direct path
- **Egocéntricas** (Hipocampo-independientes):
  - Thigmotaxis, Circling, Random path, Scanning, Chaining
- **Otras**: Perseverance

### 📈 Análisis Estadístico
- Modelos de regresión para datos longitudinales
- Tests de contingencia entre grupos
- Métricas de confianza de clasificación
- Tablas resumen por tratamiento y tiempo

## 🔧 Requisitos Técnicos

### Paquetes de R Necesarios:
- **Rtrack**: Para procesamiento de tracks MWM
- **Shiny**: Para la interfaz web (opcional)
- **dplyr, ggplot2**: Para manipulación y visualización
- **viridis**: Para paletas de colores
- **readxl**: Para leer archivos Excel

El script `install_dependencies.R` instala todo automáticamente.

## 📁 Formato de Datos

### Archivo de Experimento (.xlsx o .csv)
```
_TrackID | _TargetID | _Day | _Trial | _Arena | _TrackFile | _TrackFileFormat | Tratamiento | ...
Track_1  | Sujeto_1  |   1  |    1   | Arena_SW.txt | Track_1.csv | ethovision.3.csv | Control | ...
```

### Archivos de Tracks (.csv)
```
Time, X, Y
0.0, 50.2, 120.5
0.1, 51.3, 121.2
...
```

Ver `data/example/` para ejemplos completos.

## 🎯 Basado en tu Metodología

Esta herramienta implementa el flujo de trabajo documentado en `ejemplo.qmd`, incluyendo:

- Configuración automática de arenas
- Procesamiento con diferentes formatos de tracks
- Generación de mapas de calor como en tu análisis
- Clasificación de estrategias según tus categorías
- Análisis estadístico con modelos mixtos

## 🤝 Ventajas de esta Implementación

1. **Facilidad de uso**: Interfaz paso a paso para cualquier usuario
2. **Flexibilidad**: Tanto GUI como script para diferentes necesidades
3. **Automatización**: Elimina tareas repetitivas del flujo manual
4. **Reproducibilidad**: Configuraciones guardadas y exportables
5. **Escalabilidad**: Procesamiento paralelo para experimentos grandes
6. **Visualización**: Gráficos publicables en alta calidad

## 📚 Documentación Adicional

- `shiny_app/README.md`: Documentación detallada de la aplicación web
- `ejemplo.qmd`: Metodología y flujo de trabajo original
- Comentarios extensos en todos los scripts

## 🆘 Soporte y Troubleshooting

### Problemas Comunes:
1. **Error instalando Rtrack**: Usar `devtools::install_github("rupertoverall/Rtrack")`
2. **Archivos no encontrados**: Verificar rutas y nombres exactos
3. **Formato de tracks**: Revisar que tengan columnas Time, X, Y
4. **Memoria insuficiente**: Activar procesamiento paralelo

### Para Ayuda:
- Revisar logs de error en la aplicación
- Consultar documentación de Rtrack
- Verificar formato de datos con ejemplos incluidos

---

*Desarrollado para optimizar y facilitar el análisis de experimentos MWM, basado en metodologías establecidas y tu flujo de trabajo documentado.* 🧠🔬