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

# Procedimiento post-hoc de Bonferroni.
cat("Procedimiento post-hoc de Bonferroni\n\n")

bonferroni <- pairwise.t.test(datos[["tiempo"]],
                              datos[["algoritmo"]],
                              p.adj = "bonferroni",
                              pool.sd = TRUE,
                              paired = FALSE,
                              conf.level = 1 - alfa)

print(bonferroni)

# Procedimiento post-hoc de Holm.
cat("\n\nProcedimiento post-hoc de Holm\n\n")

holm <- pairwise.t.test(datos[["tiempo"]],
                        datos[["algoritmo"]],
                        p.adj = "holm",
                        pool.sd = TRUE,
                        paired = FALSE,
                        conf.level = 1 - alfa)

print(holm)