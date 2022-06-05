library(WRS2)
library(ggpubr)

# Construir data frame.
a <- c(25.1, 25.2, 25.3, 25.3, 25.4, 25.4, 25.5, 25.5, 25.6, 25.8, 25.8,
       25.9, 25.9, 26.0, 26.0, 26.2, 26.2, 26.2, 26.3, 26.4, 26.5, 26.5,
       26.6, 26.7, 26.7, 26.9, 26.9, 27.0, 27.1, 27.3, 27.8, 28.4, 28.5,
       29.0, 29.8, 30.2, 31.8, 31.9, 33.3, 33.7)

b <- c(24.1, 24.4, 24.4, 24.5, 24.7, 24.8, 24.8, 25.1, 25.2, 25.2, 25.2,
       25.3, 25.4, 25.7, 25.7, 26.3, 26.3, 26.4, 26.5, 27.2, 27.7, 28.3,
       28.4, 28.4, 28.6, 28.7, 29.6, 29.9, 30.1, 30.5)

tiempo <- c(a, b)
algoritmo <- c(rep("A", length(a)), rep("B", length(b)))
datos <- data.frame(tiempo, algoritmo)

# Comprobar normalidad.
g <- ggqqplot(datos, x = "tiempo", facet.by = "algoritmo",
              palette = c("blue", "red"), color = "algoritmo")

print(g)

# Establecer nivel de significación.
alfa <- 0.05

# Ver poda del 20%.
gamma <- 0.2
n_a <- length(a)
n_b <- length(b)

poda_a <- n_a * gamma
poda_b <- n_b * gamma

a_truncada <- a[poda_a:(n_a - poda_a)]
b_truncada <- b[poda_b:(n_b - poda_b)]

tiempo <- c(a_truncada, b_truncada)
algoritmo <- c(rep("A", length(a_truncada)), rep("B", length(b_truncada)))
datos_truncados <- data.frame(tiempo, algoritmo)

g <- ggqqplot(datos_truncados, x = "tiempo", facet.by = "algoritmo",
              palette = c("blue", "red"), color = "algoritmo")

print(g)

# Aplicar prueba de Yuen.
prueba <- yuen(tiempo ~ algoritmo, data = datos, tr = gamma)
print(prueba)