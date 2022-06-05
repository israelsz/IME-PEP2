library(tidyverse)
library(nlme)
library(emmeans)
library(ez)

# Crear el data frame.
instancia <- factor(1:6)
Quicksort <- c(23.2, 22.6, 23.4, 23.3, 21.8, 23.9)
Bubblesort <- c(31.6, 29.3, 30.7, 30.8, 29.8, 30.3)
Radixsort <- c(30.1, 28.4, 28.7, 28.3, 29.9, 29.1)
Mergesort <- c(25.0, 25.7, 25.7, 23.7, 25.5, 24.7)
datos <- data.frame(instancia, Quicksort, Bubblesort, Radixsort, Mergesort)

# Llevar data frame a formato largo.
datos <- datos %>% pivot_longer(c("Quicksort", "Bubblesort", "Radixsort",
                                  "Mergesort"),
                                names_to = "algoritmo", values_to = "tiempo")

datos[["algoritmo"]] <- factor(datos[["algoritmo"]])

# Nivel de significación.
alfa <- 0.01

# Procedimiento ANOVA.
anova <- ezANOVA(data = datos, dv = tiempo, within = algoritmo,
                 wid = instancia, return_aov = TRUE)

# Procedimiento post-hoc de Bonferroni.
bonferroni <- pairwise.t.test(datos[["tiempo"]], datos[["algoritmo"]],
                              p.adj = "bonferroni", paired = TRUE)

cat("Corrección de Bonferroni\n")
print(bonferroni)

# Procedimiento post-hoc de Holm.
holm <- pairwise.t.test(datos[["tiempo"]], datos[["algoritmo"]],
                        p.adj = "holm", paired = TRUE)

cat("\n\nCorrección de Holm\n")
print(holm)

# Procedimiento post-hoc HSD de Tukey.
mixto <- lme(tiempo ~ algoritmo, data = datos, random = ~1|instancia)
medias <- emmeans(mixto, "algoritmo")
tukey <- pairs(medias, adjust = "tukey")

cat("\n\nPrueba HSD de Tukey\n\n")
print(tukey)

# Procedimiento post-hoc de Scheffé
cat("\n\nComparación de Scheffé\n")
scheffe <- pairs(medias, adjust = "scheffe")
print(scheffe)