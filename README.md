# Tarea 3 — Proyecto R + Git + Excel

**Nombre:** Deilany Fuentes Barquero
**Carné:** C50177
**Curso:** CA-0204 Herramientas de Ciencia de Datos I · II Ciclo 2026
**Docente:** Luis Alberto Juárez Potoy

## Descripción
Tres ejercicios resueltos dos veces, con funciones de R y con fórmulas de Excel
parametrizadas, que deben dar el mismo resultado:
1. Alertas por ventana móvil
2. Ley de Benford por cantón
3. Valor acumulado con interés

## Estructura
- `cod/`: scripts de R (`01_datos.R` y `funciones_tarea3.R`)
- `config/`: parámetros propios del carné
- `data/`: datos generados (no se versionan)
- `excel/`: libro con la solución en Excel
- `info/figuras/`: un gráfico por ejercicio
- `Tarea3_FuentesDeilany.qmd` y `.html`: reporte y reporte renderizado

## Cómo reproducir
1. Clonar este repositorio y abrir `Tarea3_FuentesDeilany.Rproj` en RStudio.
2. Clonar la base del docente en la raíz del proyecto:
   `git clone https://github.com/PotoyJuarez/CA0204-Tarea3-base.git base`
3. Ejecutar `cod/01_datos.R` para generar `data/` y `config/`.
4. Renderizar `Tarea3_FuentesDeilany.qmd`.

## Requisitos
R, RStudio, Quarto, el paquete `ggplot2` y Excel de escritorio (Power Query).