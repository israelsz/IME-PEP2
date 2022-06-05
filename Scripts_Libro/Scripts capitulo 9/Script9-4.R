library(tidyverse)
library(DescTools)

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

# Crear matriz de contrastes.
contrastes <- matrix(c(1, -1, 0,
                       1, 0, -1,
                       0, 1, -1,
                       1, -0.5, -0.5,
                       -0.5, 1, -0.5,
                       -0.5, -0.5, 1),
                     nrow=6,
                     byrow = TRUE
)

# Trasponer matriz de contrastes (para que cada contraste sea una columna).
contrastes <- t(contrastes)

# Hacer prueba de Scheffé.
scheffe <- ScheffeTest(x = anova,
                       which = "algoritmo",
                       contrasts = contrastes,
                       conf.level = 1 - alfa
)

print(scheffe)