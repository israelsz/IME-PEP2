library(tidyverse)
library(ggpubr)
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

# Comprobción de normalidad.
g <- ggqqplot(datos, x = "tiempo", y = "algoritmo", color = "algoritmo")
g <- g + facet_wrap(~ algoritmo)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)

# Procedimiento ANOVA con aov.
cat("Procedimiento ANOVA usando aov\n\n")

prueba <- aov(tiempo ~ algoritmo + Error(instancia/(algoritmo)),
              data = datos)

print(summary(prueba))

# Procedimiento ANOVA con ezANOVA().
cat("\n\nProcedimiento ANOVA usando ezANOVA\n\n")

prueba2 <- ezANOVA(data = datos, dv = tiempo, within = algoritmo, 
                   wid = instancia, return_aov = TRUE)

print(summary(prueba2$aov))
cat("\n\nPero ezANOVA entrega más información.\n")
cat("El resultado de la prueba de esfericidad de Mauchly:\n\n")
print(prueba2[["Mauchly's Test for Sphericity"]])

cat("\n\nY factores de corrección para cuando no se cumple la\n")
cat("condición de esfericidad:\n\n")
print(prueba2$`Sphericity Corrections`)

# Gráfico del tamaño del efecto.
g2 <- ezPlot(data = datos, dv = tiempo, wid = instancia, within = algoritmo,
             y_lab = "Tiempo promedio de ejecución [ms]", x = algoritmo)

print(g2)