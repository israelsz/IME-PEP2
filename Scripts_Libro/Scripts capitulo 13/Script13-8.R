library(WRS2)

# Construir data frame.
a <- c(25.1, 25.2, 25.3, 25.3, 25.4, 25.4, 25.5, 25.5, 25.6, 25.8, 25.8,
       25.9, 25.9, 26.0, 26.0, 26.2, 26.2, 26.2, 26.3, 26.4, 26.5, 26.5,
       26.6, 26.7, 26.7, 26.9, 26.9, 27.0, 27.1, 27.3, 27.8, 28.4, 28.5,
       29.0, 29.8, 30.2, 31.8, 31.9, 33.3, 33.7)

b <- c(24.1, 24.4, 24.4, 24.5, 24.7, 24.8, 24.8, 25.1, 25.2, 25.2, 25.2,
       25.3, 25.4, 25.7, 25.7, 26.3, 26.3, 26.4, 26.5, 27.2, 27.7, 28.3,
       28.4, 28.4, 28.6, 28.7, 29.6, 29.9, 30.1, 30.5)

c <- c(24.5, 24.5, 24.5, 24.5, 24.5, 24.5, 24.6, 24.6, 24.6, 24.6, 24.6,
       24.6, 24.7, 24.7, 24.7, 24.7, 24.8, 25.0, 25.0, 25.0, 25.2, 25.2,
       25.2, 25.2, 25.5, 25.7, 25.9, 26.2, 26.5, 26.5, 26.7, 27.0, 29.2,
       29.9, 30.1)

tiempo <- c(a, b, c)
algoritmo <- c(rep("A", length(a)), rep("B", length(b)), rep("C", length(c)))
datos <- data.frame(tiempo, algoritmo)

# Fijar nivel de significación.
alfa <- 0.05

# Comparar los diferentes algoritmos usando medias truncadas.
cat("Comparación entre grupos usando medias truncadas\n\n")
gamma <- 0.2

set.seed(666)

medias_truncadas <- t1way(tiempo ~ algoritmo, data = datos, tr = gamma,
                          alpha = alfa)

print(medias_truncadas)

if(medias_truncadas$p.value < alfa) {
  cat("\nProcedimiento post-hoc\n\n")
  
  set.seed(666)
  
  post_hoc <- lincon(tiempo ~ algoritmo, data = datos, tr = gamma,
                     alpha = alfa)
  
  print(post_hoc)
}

# Comparar los diferentes algoritmos usando bootstrap.
cat("Comparación entre grupos usando bootstrap\n\n")
muestras <- 999

set.seed(666)

bootstrap <- t1waybt(tiempo ~ algoritmo, data = datos, tr = gamma,
                     nboot = muestras)

print(medias_truncadas)

if(medias_truncadas$p.value < alfa) {
  cat("\nProcedimiento post-hoc\n\n")
  
  set.seed(666)
  
  post_hoc <- mcppb20(tiempo ~ algoritmo, data = datos, tr = gamma,
                      nboot = muestras)
  
  print(post_hoc)
}