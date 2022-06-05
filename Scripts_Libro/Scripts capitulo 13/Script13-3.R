library(ggpubr)
library(rcompanion)

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

# Gráfico de dispersión e histograma.
g1 <- gghistogram(datos, x = "Population", bins = 10,
                  xlab = "Población (millones)", ylab = "Frecuencia",
                  color = "blue", fill = "blue")

g2 <- ggscatter(datos, x = "Year", y = "Population", color = "blue",
                xlab = "Año", ylab = "Población (millones)")

# Histograma de la población y población por año
original  <- ggarrange(g1, g2, ncol = 2, nrow = 1)
texto <- "Histograma de la población y población por año"
titulo  <- text_grob(texto, face = "bold", size = 14)
original <- annotate_figure(original, top = titulo)
print(original)

# Transformaciones de la población
lambda_menos_dos <- -1 / (datos$Population ** 2)
lambda_menos_uno <- -1 / datos$Population
lambda_menos_un_medio <- -1 / sqrt(datos$Population)
lambda_cero <- log(datos$Population)
lambda_un_medio <- sqrt(datos$Population)
lambda_dos <- datos$Population ** 2

transformaciones <- data.frame(datos, lambda_menos_dos, lambda_menos_uno,
                               lambda_menos_un_medio, lambda_cero,
                               lambda_un_medio, lambda_dos)

# Gráficos de dispersión para la transformación de Tukey de la población y el
# año, usando distintos valores de lambda.
gt1 <- ggscatter(transformaciones, x = "Year", y = "lambda_menos_dos",
                 color = "blue", xlab = "Año",
                 ylab = "lambda = -2") + rotate_x_text(45)

gt2 <- ggscatter(transformaciones, x = "Year", y = "lambda_menos_uno",
                 color = "blue", xlab = "Año",
                 ylab = "lambda = -1") + rotate_x_text(45)

gt3 <- ggscatter(transformaciones, x = "Year", y = "lambda_menos_un_medio",
                 color = "blue", xlab = "Año",
                 ylab = "lambda = -1/2") + rotate_x_text(45)

gt4 <- ggscatter(transformaciones, x = "Year", y = "lambda_cero",
                 color = "blue", xlab = "Año",
                 ylab = "lambda = 0") + rotate_x_text(45)

gt5 <- ggscatter(transformaciones, x = "Year", y = "lambda_un_medio",
                 color = "blue", xlab = "Año",
                 ylab = "lambda = 1/2") + rotate_x_text(45)

gt6 <- ggscatter(transformaciones, x = "Year", y = "lambda_dos",
                 color = "blue", xlab = "Año",
                 ylab = "lambda = 2") + rotate_x_text(45)

# Crear una única figura con todos los gráficos de dispersión.
dispersion  <- ggarrange(gt1, gt2, gt3, gt4, gt5, gt6, ncol = 3, nrow = 2)
texto <- "Población transformada por año"
titulo  <- text_grob(texto, face = "bold", size = 14)
dispersion <- annotate_figure(dispersion, top = titulo)
print(dispersion)

# Histogramas para la transformación de Tukey de la población y el año,
# usando distintos valores de lambda.
h1 <- gghistogram(transformaciones, bins = 10, x = "lambda_menos_dos",
                  color = "blue", fill = "blue",
                  xlab = "lambda = -2") + rotate_x_text(45)

h2 <- gghistogram(transformaciones, bins = 10, x = "lambda_menos_uno",
                  color = "blue", fill = "blue",
                  xlab = "lambda = -1") + rotate_x_text(45)

h3 <- gghistogram(transformaciones, bins = 10, x = "lambda_menos_un_medio",
                  color = "blue", fill = "blue",
                  xlab = "lambda = -1/2") + rotate_x_text(45)

h4 <- gghistogram(transformaciones, bins = 10, x = "lambda_cero",
                  color = "blue", fill = "blue",
                  xlab = "lambda = 0") + rotate_x_text(45)

h5 <- gghistogram(transformaciones, bins = 10, x = "lambda_un_medio",
                  color = "blue", fill = "blue",
                  xlab = "lambda = 1/2") + rotate_x_text(45)

h6 <- gghistogram(transformaciones, bins = 10, x = "lambda_dos",
                  color = "blue", fill = "blue",
                  xlab = "lambda = 2") + rotate_x_text(45)

# Crear una única figura con todos los gráficos de dispersión.
histograma  <- ggarrange(h1, h2, h3, h4, h5, h6, ncol = 3, nrow = 2)
texto <- "Histogramas de la población transformada"
titulo  <- text_grob(texto, face = "bold", size = 14)
histograma <- annotate_figure(histograma, top = titulo)
print(histograma)

# Buscar la mejor transformación de Tukey usando una función de R.
transformacion <- transformTukey(datos$Population, start = -4, end = 4,
                                 int = 0.001, returnLambda = TRUE)