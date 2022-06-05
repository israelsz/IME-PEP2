library(tidyverse)

# Crear el data frame en formato ancho.
A <- c(23, 19, 25, 23, 20)
B <- c(26, 24, 28, 23, 29)
C <- c(19, 24, 20, 21, 17)
datos <- data.frame(A, B, C)

# Llevar data frame a formato largo.
datos <- datos %>% pivot_longer(c("A", "B", "C"),
                                names_to = "algoritmo",
                                values_to = "tiempo")

datos[["algoritmo"]] <- factor(datos[["algoritmo"]])
datos[["instancia"]] <- factor(1:nrow(datos))

# Establecer nivel de significación (el mismo usado en ANOVA).
alfa <- 0.025

# Procedimiento ANOVA.
anova <- aov(tiempo ~ algoritmo, data = datos)

# Prueba HSD de Tukey.
post_hoc <- TukeyHSD(anova,
                     "algoritmo",
                     ordered = TRUE,
                     conf.level = 1 - alfa)

print(post_hoc)