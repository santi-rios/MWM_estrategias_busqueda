# Guía de Uso del Dataset de Prueba

Esta guía explica cómo utilizar el dataset de prueba incluido en este proyecto para probar y explorar todas las funcionalidades de la aplicación MWM Analysis Tool.

## Preparación

1. Asegúrate de que todas las dependencias estén instaladas:
   ```r
   source('shiny_app/install_dependencies.R')
   source('check_dependencies.R')  # Para verificar la instalación
   ```

2. Inicia la aplicación Shiny:
   ```r
   source('shiny_app/run_app.R')
   ```

## Paso 1: Carga de Datos

1. En la pestaña "📁 Cargar Datos":

   - En "Archivo de Experimento", haz clic en "Examinar" y selecciona:
     ```
     /data/test_dataset/MWM_Experiment_File.xlsx
     ```
     (Nota: Debe ser el archivo Excel, no el CSV)

   - En "Archivos de Tracks", haz clic en "Examinar" y selecciona los 32 archivos de track:
     ```
     /data/test_dataset/Track_1.csv
     /data/test_dataset/Track_2.csv
     ...
     /data/test_dataset/Track_32.csv
     ```
     (Puedes seleccionar varios archivos a la vez)

   - Verás la vista previa de los datos cargados con información sobre 32 tracks

## Paso 2: Configuración de Arena

1. En la pestaña "🎯 Configurar Arena":

   - Selecciona el tipo de arena: "Circular"
   - Ajusta el diámetro a 150 cm
   - Configura la plataforma:
     - Posición X: 25
     - Posición Y: 25
     - Diámetro: 20 cm

2. Haz clic en "Aplicar Configuración"

## Paso 3: Procesar Análisis

1. En la pestaña "⚙️ Procesar Análisis":

   - Activa "Procesamiento paralelo" para mayor velocidad
   - En "Configuración de Grupos":
     - Variables de agrupación: Selecciona "Group" y "Sex"
     - Variable de tratamiento: "Treatment"
     - Variable de día/tiempo: "_Day"

2. Haz clic en "🚀 Iniciar Análisis"

## Paso 4: Explorar Resultados

Una vez completado el análisis, en la pestaña "📊 Resultados", podrás explorar:

1. **Mapas de Densidad**:
   - Observa las diferentes trayectorias de nado entre grupos
   - Compara Día 1 vs Día 2 para ver la mejora en el aprendizaje
   - Compara Treatment vs Control para ver el efecto del tratamiento

2. **Estrategias de Búsqueda**:
   - Examina la clasificación de estrategias por grupo y día
   - Observa cómo las estrategias directas aumentan en el Día 2 y en el grupo de tratamiento

3. **Métricas**:
   - Analiza variables como la latencia, distancia recorrida y velocidad
   - Observa cómo mejoran las métricas a lo largo del tiempo

## Paso 5: Exportar Resultados

En la pestaña "💾 Exportar", puedes:
1. Exportar todas las gráficas generadas
2. Descargar las métricas en formato CSV
3. Guardar la clasificación de estrategias

## Interpretación de los Datos

Los datos de prueba están diseñados para mostrar patrones específicos:

1. **Efecto de aprendizaje**: Mejor rendimiento en el Día 2 vs Día 1
2. **Efecto de tratamiento**: Mejor rendimiento en el grupo Treatment vs Control
3. **Patrones de estrategias**: Evolución desde estrategias exploratorias (thigmotaxis, búsqueda aleatoria) hacia estrategias directas en el Día 2 y en grupos de tratamiento

## Personalización

Para probar diferentes escenarios:

1. Puedes modificar `MWM_Experiment_File.csv` para cambiar grupos o condiciones
2. Puedes modificar `generate_test_tracks.R` para generar nuevos patrones de nado

---

Esta guía te ayudará a explorar todas las funcionalidades de la aplicación MWM Analysis Tool utilizando los datos de prueba proporcionados.
