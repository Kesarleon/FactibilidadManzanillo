# Factibilidad Hospitalaria — Manzanillo, Colima

Este repositorio contiene una aplicación interactiva desarrollada en **R Shiny** para analizar la factibilidad de infraestructura hospitalaria en Manzanillo, Colima. La herramienta permite visualizar datos demográficos, infraestructura de salud existente, egresos hospitalarios e incidencia delictiva para apoyar la toma de decisiones.

## Características Principales

*   **Mapa Interactivo**: Visualización geoespacial de Áreas Geoestadísticas Básicas (AGEB) coloreadas por índice de demanda potencial.
    *   Capas de infraestructura existente (DENUE Salud).
    *   Visualización de egresos hospitalarios.
    *   Filtros dinámicos por índice de demanda y tipo de establecimiento.
*   **Análisis de Accesibilidad (Isócronas)**:
    *   Cálculo de zonas de cobertura de 15 y 30 minutos desde cualquier punto seleccionado en el mapa.
    *   Integración con OSRM (opcional) o fallback basado en distancia euclidiana (buffer).
*   **KPIs**: Tablero de control con indicadores clave de desempeño.
*   **Análisis de Datos**: Gráficos interactivos de series de tiempo y distribución de variables.
*   **Explorador de Datos**: Tablas interactivas para examinar los datos crudos.

## Estructura del Proyecto

El proyecto sigue una arquitectura modular para facilitar el mantenimiento y la escalabilidad:

*   `app.R`: Punto de entrada de la aplicación. Define la estructura general de la UI y el Server.
*   `global.R`: Script de configuración global. Carga librerías, define rutas de archivos y gestiona la carga de datos. **Nota**: Incluye lógica para generar datos "dummy" si no se encuentran los archivos de datos reales, permitiendo ejecutar la app en entornos de desarrollo sin datos sensibles.
*   `R/`: Directorio que contiene los módulos de la aplicación:
    *   `mod_map.R`: Lógica y visualización del mapa.
    *   `mod_kpis.R`: Cálculo y visualización de KPIs.
    *   `mod_analysis.R`: Gráficos de análisis.
    *   `mod_data.R`: Visualización tabular de datos.
    *   `utils_helpers.R`: Funciones auxiliares.
*   `data/processed/`: Directorio para almacenar los datos procesados (GeoPackages y CSV).

## Requisitos e Instalación

Para ejecutar esta aplicación localmente, necesitas tener R instalado. Se recomienda usar RStudio.

### Librerías de R

Instala las siguientes dependencias ejecutando este comando en la consola de R:

```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "leaflet",
  "sf",
  "dplyr",
  "readr",
  "plotly",
  "DT",
  "osrm" # Opcional, para isócronas precisas
))
```

## Ejecución

1.  Clona este repositorio:
    ```bash
    git clone <URL_DEL_REPOSITORIO>
    ```
2.  Abre el proyecto en RStudio o abre el archivo `app.R`.
3.  Ejecuta la aplicación haciendo clic en el botón **"Run App"** en RStudio o ejecutando:
    ```r
    shiny::runApp()
    ```

### Datos

La aplicación espera encontrar los siguientes archivos en `data/processed/`:
*   `ageb_factibilidad.gpkg`: Datos de AGEBs con scores de factibilidad.
*   `denue_salud.gpkg`: Puntos de establecimientos de salud (DENUE).
*   `egresos_all.csv`: Histórico de egresos hospitalarios.
*   `delitos_manzanillo.csv`: Datos de incidencia delictiva.

**Nota Importante**: Si estos archivos no existen, `global.R` generará automáticamente datos simulados (dummy) para permitir la exploración de la funcionalidad de la aplicación.

## Créditos

Equipo de Desarrollo.
Consultoría: Kesarleon AI
