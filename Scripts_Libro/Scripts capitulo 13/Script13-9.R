library(WRS2)
library(tidyverse)

# Construir data frame.
X <- c(32.0, 32.0, 32.0, 32.0, 32.1, 32.1, 32.1, 32.2, 32.3, 32.3, 32.5,
       32.7, 32.7, 32.7, 33.1, 33.4, 33.9, 34.1, 34.2, 34.5, 36.0, 36.6,
       36.7, 37.2, 38.0)

Y <- c(33.0, 33.0, 33.0, 33.0, 33.0, 33.0, 33.3, 33.3, 33.3, 33.3, 33.5,
       33.6, 33.7, 33.9, 33.9, 34.2, 34.2, 34.3, 34.3, 34.4, 34.5, 34.6,
       36.4, 38.9, 40.2)

Z <- c(32.0, 32.2, 32.5, 32.6, 32.7, 32.7, 32.7, 33.0, 33.2, 33.4, 33.6,
       33.6, 33.9, 34.1, 34.2, 34.4, 34.4, 34.5, 34.6, 34.7, 36.3, 36.6,
       36.7, 38.9, 39.2)

instancia <- 1:length(X)
datos <- data.frame(instancia, X, Y, Z)

# Llevar data frame a formato largo.
datos <- datos %>% pivot_longer(c("X", "Y", "Z"), names_to = "algoritmo",
                                values_to = "tiempo")

datos[["algoritmo"]] <- factor(datos[["algoritmo"]])

# Fijar nivel de significación.
alfa <- 0.05

# Aplicar alternativa robusta para ANOVA de una vía con
# muestras correlacionadas.
gamma <- 0.2

prueba <- rmanova(y = datos[["tiempo"]], groups = datos[["algoritmo"]],
                  blocks = datos[["instancia"]], tr = gamma)

print(prueba)

if(prueba$p.value < alfa) {
  cat("\nProcedimiento post-hoc\n\n")
  
  post_hoc <- rmmcp(y = datos[["tiempo"]], groups = datos[["algoritmo"]],
                    blocks = datos[["instancia"]], tr = gamma, alpha = alfa)
  
  print(post_hoc)
}