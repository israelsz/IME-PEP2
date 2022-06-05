library(WRS2)

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

# Establecer nivel de significación y cantidad de muestras a generar
# con bootstrapping.
alfa <- 0.05
bootstrap <- 999

# Aplicar prueba con la media
set.seed(135)

prueba_media <- pb2gen(tiempo ~ algoritmo,
                       data = datos,
                       est = "mean",
                       nboot = bootstrap)

cat("\n\nResultado al usar la media como estimador\n\n")
print(prueba_media)

# Aplicar prueba con la mediana
set.seed(135)

prueba_mediana <- pb2gen(tiempo ~ algoritmo,
                         data = datos,
                         est = "median",
                         nboot = bootstrap)

cat("Resultado al usar la mediana como estimador\n\n")
print(prueba_mediana)