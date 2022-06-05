library(boot)
library(bootES)

# Crear muestra inicial, mostrar su histograma y calcular la media.
muestra <- c(79, 75, 84, 75, 94, 82, 76, 90, 79, 88)
datos <- data.frame(muestra)

# Establecer cantidad de remuestreos y nivel de significación.
B = 2000
alfa <- 0.01

cat("Paquete boot\n")

# Construir distribución bootstrap usando el paquete boot.
media <- function(valores, i) {
  mean(valores[i])
}

set.seed(432)
distribucion_b <- boot(muestra, statistic = media, R = B)
print(distribucion_b)

# Graficar distribución bootstrap.
print(plot(distribucion_b))

# Construir intervalos de confianza.
intervalo_t <- boot.ci(distribucion_b, conf = 1 - alfa, type = "norm")

cat("\n\nIntervalo de confianza usando distribución t:\n")
print(intervalo_t)

intervalo_per <- boot.ci(distribucion_b, conf = 1 - alfa, type = "perc")

cat("\n\nIntervalo de confianza usando percentiles:\n")
print(intervalo_per)

intervalo_bca <- boot.ci(distribucion_b, conf = 1 - alfa, type = "bca")

cat("\n\nIntervalo de confianza BCa:\n")
print(intervalo_bca)

# Construir distribución bootstrap usando el paquete bootES.
set.seed(432)

distribucion_bootstrapES <- bootES(muestra, R = B, ci.type = "bca",
                                   ci.conf = 1 - alfa, plot = TRUE)

print(distribucion_bootstrapES)