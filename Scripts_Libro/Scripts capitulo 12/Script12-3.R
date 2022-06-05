library(simpleboot)
library(boot)
library(ggpubr)

set.seed(432)

# Ingresar datos originales
hombres <- c(1.3, 1.5, 1.6, 1.7, 1.7, 1.9, 2.3, 2.4, 2.6, 2.6, 2.7,
             2.8, 3.2, 3.7, 4.1, 4.4, 4.5, 4.8, 5.2, 5.2, 5.3, 5.5,
             5.5, 5.6, 5.6, 5.7, 5.7)

mujeres <- c(3.5, 3.6, 3.8, 4.3, 4.5, 4.5, 4.9, 5.1, 5.3, 5.3, 5.5,
             5.8, 6.0, 6.3, 6.3, 6.4, 6.4, 6.6, 6.7)

n_hombres <- length(hombres)
n_mujeres <- length(mujeres)

sexo <- c(rep("Hombre", n_hombres), rep("Mujer", n_mujeres))
nota <- c(hombres, mujeres)
datos <- data.frame(nota, sexo)

# Comprobar normalidad de las muestras.
print(shapiro.test(hombres))
print(shapiro.test(mujeres))

# Calcular la diferencia observada entre las medias muestrales.
media_hombres <- mean(hombres)
media_mujeres <- mean(mujeres)
diferencia_observada <- media_hombres - media_mujeres

cat("diferencia observada:", media_hombres - media_mujeres, "\n\n")

# Establecer el nivel de significación.
alfa <- 0.05

# Crear la distribución bootstrap.
B <- 9999
distribucion_bootstrap <- two.boot(hombres, mujeres, FUN = mean, R = B)

# Examinar la distribución bootstrap.
valores <- data.frame(distribucion_bootstrap$t)
colnames(valores) <- "valores"

histograma <- gghistogram(valores, x = "valores", color = "red",
                          fill = "red", bins = 100,
                          xlab = "Diferencia de medias",
                          ylab = "Frecuencia", add = "mean")

print(histograma)

qq <- ggqqplot(valores, x = "valores", color = "red")
print(qq)

cat("Distribución bootstrap:\n")
cat("\tMedia:", mean(valores$valores), "\n")
cat("\tDesviación estándar:", sd(valores$valores), "\n\n")

# Construir el intervalo de confianza.
intervalo_bca <- boot.ci(distribucion_bootstrap, conf = 1 - alfa,
                         type = "bca")

print(intervalo_bca)