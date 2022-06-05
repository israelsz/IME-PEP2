library(bootES)

set.seed(432)

# Ingresar datos originales.
alumno <- 1:20

prueba_1 <- c(3.5, 2.7, 1.0, 1.8, 1.6, 4.3, 5.8, 6.4, 3.9, 4.3, 3.4,
              5.3, 5.8, 5.3, 2.0, 1.3, 4.0, 5.3, 1.6, 3.6)

prueba_2 <- c(5.2, 5.1, 5.9, 4.8, 1.4, 2.3, 6.8, 5.3, 3.1, 3.8, 4.6,
              1.2, 3.9, 2.0, 1.7, 3.3, 6.0, 4.8, 6.9, 1.3)

# Establecer nivel de significación.
alfa <- 0.05

# Calcular la diferencia entre ambas observaciones.
diferencia <- prueba_2 - prueba_1

# Generar la distribución bootstrap y su intervalo de confianza.
B <- 3999

distribucion_bootstrapES <- bootES(diferencia, R = B, ci.type = "bca",
                                   ci.conf = 1 - alfa, plot = FALSE)

print(distribucion_bootstrapES)