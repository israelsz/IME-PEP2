library(boot)

set.seed(432)

# Crear muestra inicial, mostrar su histograma y calcular la media.
muestra <- c(79, 75, 84, 75, 94, 82, 76, 90, 79, 88)
valor_observado <- mean(muestra)
datos <- data.frame(muestra)

# Construir distribución bootstrap.
B <- 2000

media <- function(valores, i) {
  mean(valores[i])
}

distribucion_b <- boot(muestra, statistic = media, R = B)

# Desplazar la distribución bootstrap para que se centre en
# el valor nulo.
valor_nulo <- 75
desplazamiento <- mean(distribucion_b[["t"]]) - valor_nulo
distribucion_nula <- distribucion_b[["t"]] - desplazamiento

# Determinar el valor p.
p <- (sum(distribucion_nula > valor_observado) + 1) / (B + 1)
cat("Valor p:", p)