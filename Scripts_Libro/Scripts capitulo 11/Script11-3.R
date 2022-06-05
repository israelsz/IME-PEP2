# Construir la mariz de datos.
A <- c(24, 23, 26, 21, 24, 24, 25, 22, 23, 22, 23, 23)
B <- c(17, 15, 18, 20, 19, 21, 20, 18, 19)
C <- c(10, 11, 14, 11, 15, 12, 12, 10, 9, 13, 12, 12, 10, 10)
D <- c(18, 16, 18, 15, 16, 15, 18, 16)
Tiempo <- c(A, B, C, D)

Algoritmo <- c(rep("A", length(A)),
               rep("B", length(B)),
               rep("C", length(C)),
               rep("D", length(D)))

Algoritmo <- factor(Algoritmo)

datos <- data.frame(Tiempo, Algoritmo)

# Establecer nivel de significación
alfa <- 0.01

# Hacer la prueba de Kruskal-Wallis.
prueba <- kruskal.test(Tiempo ~ Algoritmo, data = datos)
print(prueba)

# Efectuar procedimiento post-hoc de Holm si se encuentran diferencias
# significativas.
if(prueba$p.value < alfa) {
  post_hoc <- pairwise.wilcox.test(datos$Tiempo,
                                   datos$Algoritmo,
                                   p.adjust.method = "holm",
                                   paired = FALSE)
  
  print(post_hoc)
}