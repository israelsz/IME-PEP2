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

# Calcular la diferencia observada entre las medias muestrales.
media_hombres <- mean(hombres)
media_mujeres <- mean(mujeres)
valor_observado <- media_hombres - media_mujeres

# Crear la distribución bootstrap.
B <- 9999
valor_nulo <- 1.5
distribucion_bootstrap <- two.boot(hombres, mujeres, FUN = mean, R = B)
desplazamiento <- mean(distribucion_bootstrap[["t"]]) - valor_nulo
distribucion_nula <- distribucion_bootstrap[["t"]] - desplazamiento

# Determinar el valor p.
p <- (sum(abs(distribucion_nula) > abs(valor_observado)) + 1) / (B + 1)
cat("Valor p:", p)