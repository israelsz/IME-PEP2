# Construir la mariz de datos.
A <- c(21, 10, 7, 21, 24, 27, 17)
B <- c(6, 21, 18, 7, 24, 13, 13)
C <- c(13, 25, 18, 20, 24, 8, 29)

Puntuacion <- c(A, B, C)

Interfaz <- c(rep("A", length(A)),
              rep("B", length(B)),
              rep("C", length(C)))

Sujeto <- rep(1:7, 3)

Interfaz <- factor(Interfaz)

datos <- data.frame(Sujeto, Puntuacion, Interfaz)

# Establecer nivel de significación
alfa <- 0.05

# Hacer la prueba de Friedman.
prueba <- friedman.test(Puntuacion ~ Interfaz | Sujeto, data = datos)
print(prueba)

# Efectuar procedimiento post-hoc de Holm si se encuentran diferencias
# significativas.
if(prueba$p.value < alfa) {
  post_hoc <- pairwise.wilcox.test(datos$Puntuacion,
                                   datos$Interfaz,
                                   p.adjust.method = "holm",
                                   paired = TRUE)
  
  print(post_hoc)
}