library(tidyverse)
library(ggpubr)
library(ez)

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

# Comprobción de normalidad.
g <- ggqqplot(datos,
              x = "tiempo",
              y = "algoritmo",
              color = "algoritmo")

g <- g + facet_wrap(~ algoritmo)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)

# Procedimiento ANOVA con aov().
cat("Procedimiento ANOVA usando aov\n\n")
prueba <- aov(tiempo ~ algoritmo, data = datos)
print(summary(prueba))

# Procedimiento ANOVA con ezANOVA().
cat("\n\nProcedimiento ANOVA usando ezANOVA\n\n")
prueba2 <- ezANOVA(
  data = datos,
  dv = tiempo,
  between = algoritmo,
  wid = instancia,
  return_aov = TRUE)

print(prueba2)

# Gráfico del tamaño del efecto.
g2 <- ezPlot(
  data = datos,
  dv = tiempo,
  wid = instancia,
  between = algoritmo,
  y_lab = "Tiempo promedio de ejecución [ms]",
  x = algoritmo
)

print(g2)