library(ggpubr)
library(DescTools)

# Cargar datos
Year <- c(1610, 1620, 1630, 1640, 1650, 1660, 1670, 1680, 1690, 1700, 1710,
          1720, 1730, 1740, 1750, 1760, 1770, 1780, 1790, 1800, 1810, 1820,
          1830, 1840, 1850)

Population <- c(0.00035, 0.002302, 0.004646, 0.026634, 0.050368, 0.075058,
                0.111935, 0.151507, 0.210372, 0.250888, 0.331711, 0.466185,
                0.629445, 0.905563, 1.17076, 1.593625, 2.148076, 2.780369,
                3.929214, 5.308483, 7.239881, 9.638453, 12.86602, 17.069453,
                23.191876)

datos <- data.frame(Year, Population)

# Transformación de Box-cox
box_cox <- function(x, lambda) {
  if(lambda == 0) {
    return(log(x))
  }
  
  resultado <- (x ** lambda -1) / lambda
  return(resultado)
}

# Transformaciones de la población
lambda_menos_dos <- box_cox(datos$Population, -2)
lambda_menos_uno <- box_cox(datos$Population, -1)
lambda_cero <- box_cox(datos$Population, 0)
lambda_uno <- box_cox(datos$Population, 1)
lambda_dos <- box_cox(datos$Population, 2)

transformaciones <- data.frame(datos, lambda_menos_dos, lambda_menos_uno,
                               lambda_cero, lambda_uno, lambda_dos)

# Gráficos de dispersión para la transformación de Box-Cox de la población y
# el año, usando distintos valores de lambda.
gt1 <- ggscatter(transformaciones, x = "Year", y = "lambda_menos_dos",
                 color = "purple", xlab = "Año",
                 ylab = "lambda = -2") + rotate_x_text(45)

gt2 <- ggscatter(transformaciones, x = "Year", y = "lambda_menos_uno",
                 color = "purple", xlab = "Año",
                 ylab = "lambda = -1") + rotate_x_text(45)

gt3 <- ggscatter(transformaciones, x = "Year", y = "lambda_cero",
                 color = "purple", xlab = "Año",
                 ylab = "lambda = 0") + rotate_x_text(45)

gt4 <- ggscatter(transformaciones, x = "Year", y = "lambda_uno",
                 color = "purple", xlab = "Año",
                 ylab = "lambda = 1") + rotate_x_text(45)

gt5 <- ggscatter(transformaciones, x = "Year", y = "lambda_dos",
                 color = "purple", xlab = "Año",
                 ylab = "lambda = 2") + rotate_x_text(45)

# Crear una única figura con todos los gráficos de dispersión.
dispersion  <- ggarrange(gt1, gt2, gt3, gt4, gt5, ncol = 3, nrow = 2)
texto <- "Población transformada por año"
titulo  <- text_grob(texto, face = "bold", size = 14)
dispersion <- annotate_figure(dispersion, top = titulo)
print(dispersion)

# Buscar la mejor transformación Box-Cox usando funciones de R.
lambda <- BoxCoxLambda(datos$Population, lower = -4, upper = 4)
cat("Lambda óptimo:", lambda)
transformacion <- BoxCox(datos$Population, lambda)
datos <- data.frame(datos, transformacion)

# Graficar los datos transformados.
g1 <- ggqqplot(transformacion, color = "purple")
print(g1)

g2 <- gghistogram(datos, bins = 10, x = "transformacion", color = "purple",
                  fill = "purple", xlab = "Población (Box-Cox)",
                  ylab = "Frecuencia") + rotate_x_text(45)

print(g2)

# Gráfico de dispersión para la transformación de Box-Cox de la población y
# el año, usando lambda óptimo.
g3 <- ggscatter(datos, x = "Year", y = "transformacion", color = "purple",
                xlab = "Año", ylab = "lambda óptimo") + rotate_x_text(45)

print(g3)