# PEP N°2 - Forma 3

#Importación de paquetes

if(!require(ggpubr)){
  install.packages("ggpubr",dependencies = TRUE)
  require(ggpubr)
}

if(!require(simpleboot)){
  install.packages("simpleboot",dependencies = TRUE)
  require(simpleboot)
}

if(!require(boot)){
  install.packages("boot",dependencies = TRUE)
  require(boot)
}

# Se fija una semilla
set.seed(2349)

# Creación del dataset de acuerdo a los datos entregados
materna <- c(87.9, 92.5, 88.1, 89.9, 87.8, 92.6, 88.0, 91.2, 86.8, 87.0, 88.0, 
             91.3, 89.2, 88.2)
formula <- c(91.7, 87.9, 87.6, 88.1, 89.2, 87.4, 93.8, 89.1, 88.4, 89.4, 91.8,
             87.0, 87.7, 89.5, 87.9, 88.2)

n_materna <- length(materna)
n_formula <- length(formula)

leche <- c(rep("Materna", n_materna), rep("Fórmula", n_formula))
estatura <- c(materna, formula)
datos <- data.frame(estatura, leche)

# En primer lugar, se establecen las hipótesis sobre las cuales se busca inferir
# para esta muestra:

# Hipótesis:
# H0: No existe diferencia en la estatura promedio entre los niños varones de dos
#     años que se alimentan con leche materna con los que se alimentan con
#     leche de fórmula.
# Ha: Existe diferencia en la estatura promedio entre los niños varones de dos
#     años que se alimentan con leche materna con los que se alimentan con 
#     leche de fórmula.

# Matemáticamente:
#  Sea μm la media de la estatura de los niños de dos años que se alimentan de
# leche materna y μf la media de la estatura de los niños de dos años que se
# alimentan de leche de fórmula:
# H0: μm – μf = 0
# Ha: μm – μf != 0

# Debido a que lo que se busca conocer es la diferencia entre medias de estos
# dos grupos es que resulta como primera alternativa pensar en una prueba t de
# student para resolver el problema, por lo que se procederá a probar sus
# condiciones: Los datos son independientes, debido a que se trata de un 
# estudio y se puede asumir que las muestras de los participantes fueron
# seleccionadas aleatoriamente, además la muestra representa a menos del
# 10% de la población.
# A continuación, se comprobará la normalidad de las muestras, para probar la
# segunda condición de la prueba T de dos muestras independientes.

print(shapiro.test(materna))
print(shapiro.test(formula))

# Es posible observar que para la muestra de niños que consumen leche materna
# arroja un valor p de 0.046 en la prueba de Shapiro y de 0.01 para la muestra
# de niños que consumen leche de fórmula, por lo que se falla en rechazar la
# hipótesis nula de la prueba de Shapiro y Se puede concluir con 95% de 
# confianza que no cumplen el supuesto de normalidad y por ende no es posible
# aplicar la prueba T de student.

# Ante esto es que se hace necesario el aplicar una técnica de remuestreo como
# bootstrapping que es capaz de ayudar con estos datos problemáticos, generando
# una mayor cantidad de datos a partir de las muestras que se tienen, teniendo
# la posibilidad de lograr cumplir la condición de normalidad en este caso y de
# esta forma poder inferir sobre una muestra que antes no era posible inferir.

# Bajo lo expuesto se procede a generar aplicar la técnica de bootstrapping, 
# donde el estadístico que se busca remuestrear corresponde a la media,
# debido a que se solicita inferir respecto a promedios.

# Se calcula la diferencia observada entre las medias muestrales.
media_estatura_materna <- mean(materna)
media_estatura_formula <- mean(formula)
valor_observado <- media_estatura_materna - media_estatura_formula

# Se establece el nivel de significación.
alfa <- 0.05

# Crear la distribución bootstrap.
B <- 9999
valor_nulo <- 0
distribucion_bootstrap <- two.boot(materna, formula, FUN = mean, R = B)

# Se estudia la distribución bootstrap generada a través de gráficos
valores <- data.frame(distribucion_bootstrap$t)
colnames(valores) <- "valores"

#Histograma
histograma <- gghistogram(valores, x = "valores", color = "red",
                          fill = "red", bins = 100,
                          xlab = "Diferencia de medias",
                          ylab = "Frecuencia", add = "mean")

print(histograma)

# Grafico Q-Q
qq <- ggqqplot(valores, x = "valores", color = "red")
print(qq)

# Del histograma es posible observar como la forma de los datos describe una 
# distribución normal, además el grafico Q-Q no muestra ningún dato atípico, 
#es por esto que es posible afirmar que los datos remuestreados ahora si 
#cumplen con el supuesto de normalidad.

# Media y desviación de la distribución bootstrap generada
cat("Distribución bootstrap:\n")
cat("\tMedia:", mean(valores$valores), "\n")
cat("\tDesviación estándar:", sd(valores$valores), "\n\n")

# Construcción del intervalo de confianza.
intervalo_bca <- boot.ci(distribucion_bootstrap, conf = 1 - alfa,
                         type = "bca")

print(intervalo_bca)


# Calculo de la prueba de hipotesis

desplazamiento <- mean(distribucion_bootstrap[["t"]]) - valor_nulo
distribucion_nula <- distribucion_bootstrap[["t"]] - desplazamiento

# Se calcula el valor p.
p <- (sum(abs(distribucion_nula) > abs(valor_observado)) + 1) / (B + 1)
cat("Valor p:", p)

# La diferencia entre las medias tiene un valor de 0.1354 y se encuentra dentro
# del intervalo de confianza [-1.2357, 1.4616], además tomando en consideración
# el valor p obtenido igual a 0.8445, resultado bastante mayor al nivel de
# significación alfa = 0.05 por lo que se falla en rechazar la hipótesis nula.
# En conclusión, es posible afirmar con 95% de confianza que no existe
# diferencia en las estaturas promedio entre los niños varones de dos años que
# se alimentan con leche materna con los que se alimentan con leche de fórmula.