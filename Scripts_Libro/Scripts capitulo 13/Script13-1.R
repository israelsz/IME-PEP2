# Crear un vector con cuatro observaciones en grados Celsius.
Celsius <- c(-8, 0, 29.8, 100)

# Aplicar transformación lineal para convertir a grados Fahrenheit.
Fahrenheit <- 1.8 * Celsius + 32

# Mostrar los resultados.
cat("Temperaturas en grados Celsius\n")
print(Celsius)
cat("\nTemperaturas en grados Fahrenheit\n")
print(Fahrenheit)