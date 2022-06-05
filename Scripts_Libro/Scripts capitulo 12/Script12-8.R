library(ggpubr)
library(ez)
library(tidyverse)

# Crear el data frame.
Quicksort <- c(11.2, 22.6, 23.4, 23.3, 21.8, 40.1)
Bubblesort <- c(15.7, 29.3, 30.7, 30.8, 29.8, 50.3)
Mergesort <- c(12.0, 25.7, 25.7, 23.7, 25.5, 44.7)
Instancia <- factor(1:6)
datos_anchos <- data.frame(Instancia, Quicksort, Bubblesort, Mergesort)

datos_largos <- datos_anchos %>% pivot_longer(c("Quicksort", "Bubblesort",
                                                "Mergesort"),
                                              names_to = "Algoritmo",
                                              values_to = "Tiempo")

datos_largos[["Algoritmo"]] <- factor(datos_largos[["Algoritmo"]])

# Verificar condición de normalidad.
g <- ggqqplot(datos_largos, "Tiempo", facet.by = "Algoritmo",
              color = "Algoritmo")

print(g)

# Establecer nivel de significación.
alfa <- 0.01

# Obtener el valor observado, correspondiente al estadístico F entregado
# por ANOVA para la muestra original.
anova <- ezANOVA(datos_largos, dv = Tiempo, within = Algoritmo,
                 wid = Instancia, return_aov = TRUE)

valor_observado <- anova[["ANOVA"]][["F"]]

# Generar permutaciones.
R = 2999
# copia_ancha <- data.frame(datos_anchos)

set.seed(432)

# Función para obtener una permutación.
# Devuelve una matriz de datos con formato ancho.
obtiene_permutacion <- function(i, df_ancho) {
  df_ancho[, 2:4] <- t(apply(df_ancho[, 2:4], 1, sample))
  return(df_ancho)
}

# Obtiene permutaciones
permutaciones <- lapply(1:R, obtiene_permutacion, datos_anchos)

# Función para obtener el estadístico F para una matriz de datos con formato
# ancho.
obtiene_F <- function(df_ancho) {
  df_largo <- df_ancho %>% pivot_longer(c("Quicksort", "Bubblesort",
                                          "Mergesort"),
                                        names_to = "Algoritmo",
                                        values_to = "Tiempo")
  
  df_largo[["Algoritmo"]] <- factor(df_largo[["Algoritmo"]])
  
  anova <- ezANOVA(df_largo, dv = Tiempo, within = Algoritmo, wid = Instancia,
                   return_aov = TRUE)
  return(anova[["ANOVA"]][["F"]])
}

# Genera distribución de estadísticos F con las permutaciones.
distribucion <- sapply(permutaciones, obtiene_F)

# Obtener valor p.
p <- (sum(distribucion > valor_observado) + 1) / (R + 1)
cat("ANOVA de una vía para muestras pareadas con permutaciones\n")
cat("p =", p, "\n\n")

# Análisis post-hoc.

# Función para calcular la media de las diferencias para dos columnas de una
# matriz de datos en formato ancho.
obtiene_media_difs <- function(df_ancho, columna_1, columna_2) {
  media <- mean(df_ancho[[columna_1]] - df_ancho[[columna_2]])
  return(media)
}

# Obtiene las las medias de las diferencias observadas
dif_obs_quick_bubble <- obtiene_media_difs(datos_anchos, "Quicksort",
                                           "Bubblesort")

dif_obs_quick_merge <- obtiene_media_difs(datos_anchos, "Quicksort",
                                          "Mergesort")

dif_obs_bubble_merge <- obtiene_media_difs(datos_anchos, "Bubblesort",
                                           "Mergesort")

# Obtiene las distribuciones de las medias de las diferencias permutadas
dist_medias_difs_quick_bubble <- sapply(permutaciones, obtiene_media_difs,
                                        "Quicksort", "Bubblesort")

dist_medias_difs_quick_merge <- sapply(permutaciones, obtiene_media_difs,
                                       "Quicksort", "Mergesort")

dist_medias_difs_bubble_merge <- sapply(permutaciones, obtiene_media_difs,
                                        "Bubblesort", "Mergesort")

# Obtener valores p.
num <- sum(abs(dist_medias_difs_quick_bubble) > abs(dif_obs_quick_bubble)) + 1
den <- R + 1
p_quick_bubble <- num / den

num <- sum(abs(dist_medias_difs_quick_merge) > abs(dif_obs_quick_merge)) + 1
den <- R + 1
p_quick_merge <- num / den

num <- sum(abs(dist_medias_difs_bubble_merge) > abs(dif_obs_bubble_merge)) + 1
den <- R + 1
p_bubble_merge <- num / den

cat("\n\n")
cat("Análisis post-hoc (permutaciones) para la diferencia de las medias\n")
cat("---------------------------------------------------------\n")
cat("Valores p:\n")

cat(sprintf("Quicksort - Bubblesort: %.3f\n", p_quick_bubble))
cat(sprintf("Quicksort - Mergesort: %.3f\n", p_quick_merge))
cat(sprintf("Bubblesort - Mergesort: %.3f\n", p_bubble_merge))

cat("\nDiferencias observadas:\n")
cat(sprintf("Quicksort - Bubblesort: %.3f\n", dif_obs_quick_bubble))
cat(sprintf("Quicksort - Mergesort: %.3f\n", dif_obs_quick_merge))
cat(sprintf("Bubblesort - Mergesort: %.3f\n", dif_obs_bubble_merge))

