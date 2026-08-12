# mixcouples_latam

Parejas mixtas en América Latina — estudio de emparejamiento selectivo (assortative mating) y homofilia en parejas convivientes a partir de censos latinoamericanos, con foco en el emparejamiento nativo/no nativo.

## Estructura

```
chile/
  random_forest_censo2024.R           # construcción de datos, clustering K-Means y pipeline de Random Forest
  reporte_parejas_censo2024.qmd       # reporte Quarto: tipologías de parejas, Censo Chile 2024
  reporte_parejas_censo2024.html      # reporte renderizado
  chi_couples_stats.rds               # dataset a nivel de pareja (input del reporte .qmd)
  diccionario_variables_censo2024.xlsx # diccionario de variables, Censo 2024
```

Cada país tiene su propia carpeta de nivel superior (ej. `chile/`), siguiendo el mismo patrón: un script de construcción, un dataset a nivel de pareja, un reporte Quarto, y los diccionarios de datos fuente.

## Chile — Censo 2024

Explora tipologías de parejas convivientes a partir del Censo de Población y Vivienda 2024 de Chile. Dos preguntas:

1. ¿Qué tipologías de parejas convivientes existen en Chile en 2024?
2. ¿Qué factores sociodemográficos determinan más el emparejamiento entre personas nativas y no nativas?

**Pipeline** (`random_forest_censo2024.R`):
- Recodifica variables censales a nivel individual (educación, situación laboral, ocupación, lugar de nacimiento, religión, hijos).
- Construye parejas mediante inner join entre jefes de hogar (`parentesco == 1`) y cónyuges/convivientes (`parentesco %in% c(2,3,4)`).
- Deriva variables a nivel de díada: diferencia de edad, educación conjunta, origen conjunto, homogamia religiosa, situación laboral conjunta, ocupación conjunta, presencia de hijos, área.
- Excluye parejas del mismo sexo (ver reporte para justificación).
- Output: `chi_couples_stats.rds`.

**Análisis** (`reporte_parejas_censo2024.qmd`):
- Clustering K-Means (k=5, validado con elbow test) sobre tipologías de pareja, corrido en tres configuraciones de muestra: parejas con migrantes, muestra completa, parejas homógamas en origen.
- Random Forest (`ranger`) prediciendo `native_couple`, mismas tres configuraciones, para ordenar predictores del emparejamiento mixto.
- Hallazgo clave: la ocupación conjunta es el predictor dominante en todas las configuraciones; la religión sube al segundo lugar solo en la submuestra de parejas con migrantes.

Renderizar el reporte con:

```r
quarto::quarto_render("chile/reporte_parejas_censo2024.qmd")
```

Requiere: `tidyverse`, `fastDummies`, `cluster`, `ranger`, `ggplot2`, `poLCA`, `vip`, `pROC`.
