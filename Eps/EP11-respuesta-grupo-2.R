# Ejercicio práctico N°11
# Grupo N°2
# Integrantes:
# Christofer Rodriguez - Christian Méndez  - Israel Arias

#Importación de paquetes
if(!require(tidyverse)){
  install.packages("tidyverse",dependencies = TRUE)
  require(tidyverse)
}

if(!require(ggpubr)){
  install.packages("ggpubr",dependencies = TRUE)
  require(ggpubr)
}

if(!require(ez)){
  install.packages("ez",dependencies = TRUE)
  require(ez)
}

##################### Funciones para aplicar Montecarlo #######################
# Función para obtener una permutación.
# Argumentos:
# - i: iterador (para llamadas posteriores).
# - muestra_1, muestra_2: muestras.
# Valor:
# - lista con las muestras resultantes tras la permutación.
obtiene_permutacion <- function(i, muestra_1, muestra_2) {
  n_1 <- length(muestra_1)
  combinada <- c(muestra_1, muestra_2)
  n <- length(combinada)
  permutacion <- sample(combinada, n, replace = FALSE)
  nueva_1 <- permutacion[1:n_1]
  nueva_2 <- permutacion[(n_1+1):n]
  return(list(nueva_1, nueva_2))
}

# Función para calcular la diferencia de un estadístico de interés entre las
# dos muestras.
# Argumentos:
# - muestras: lista con las muestras.
# - FUN: nombre de la función que calcula el estadístico de interés.
# Valor:
# - diferencia de un estadístico para dos muestras.
calcular_diferencia <- function(muestras, FUN) {
  muestra_1 <- muestras[[1]]
  muestra_2 <- muestras[[2]]
  diferencia <- FUN(muestra_1) - FUN(muestra_2)
  return(diferencia)
}

# Función para calcular el valor p.
# Argumentos:
# - distribucion: distribución nula del estadístico de interés.
# - valor_observado: valor del estadístico de interés para las muestras
#   originales.
# - repeticiones: cantidad de permutaciones a realizar.
# - alternative: tipo de hipótesis alternativa. "two.sided" para
#   hipótesis bilateral, "greater" o "less" para hipótesis unilaterales.
# Valor:
# - el valorp calculado.
calcular_valor_p <- function(distribucion, valor_observado,
                             repeticiones, alternative) {
  if(alternative == "two.sided") {
    numerador <- sum(abs(distribucion) > abs(valor_observado)) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  else if(alternative == "greater") {
    numerador <- sum(distribucion > valor_observado) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  else {
    numerador <- sum(distribucion < valor_observado) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  
  return(valor_p)
}

# Función para graficar una distribución.
# Argumentos:
# - distribucion: distribución nula del estadístico de interés.
# - ...: otros argumentos a ser entregados a gghistogram y ggqqplot.
graficar_distribucion <- function(distribucion, ...) {
  observaciones <- data.frame(distribucion)
  
  histograma <- gghistogram(observaciones, x = "distribucion",
                            xlab = "Estadístico de interés",
                            ylab = "Frecuencia", bins = 30, ...)
  
  qq <- ggqqplot(observaciones, x = "distribucion", ...)
  
  # Crear una única figura con todos los gráficos de dispersión.
  figura  <- ggarrange(histograma, qq, ncol = 2, nrow = 1)
  print(figura)
}

# Función para hacer la prueba de permutaciones.
# Argumentos:
# - muestra_1, muestra_2: vectores numéricos con las muestras a comparar.
# - repeticiones: cantidad de permutaciones a realizar.
# - FUN: función del estadístico E para el que se calcula la diferencia.
# - alternative: tipo de hipótesis alternativa. "two.sided" para
#   hipótesis bilateral, "greater" o "less" para hipótesis unilaterales.
# - plot: si es TRUE, construye el gráfico de la distribución generada.
# - ...: otros argumentos a ser entregados a graficar_distribucion.
contrastar_hipotesis_permutaciones <- function(muestra_1, muestra_2,
                                               repeticiones, FUN,
                                               alternative, plot, ...) {
  cat("Prueba de permutaciones\n\n")
  cat("Hipótesis alternativa:", alternative, "\n")
  observado <- calcular_diferencia(list(muestra_1, muestra_2), FUN)
  cat("Valor observado:", observado, "\n")
  
  n_1 <- length(muestra_1)
  
  # Generar permutaciones.
  permutaciones <- lapply(1:repeticiones, obtiene_permutacion, muestra_1,
                          muestra_2)
  
  # Generar la distribución.
  distribucion <- sapply(permutaciones, calcular_diferencia, FUN)
  
  # Graficar la distribución.
  if(plot) {
    graficar_distribucion(distribucion, ...)
  }
  
  # Calcular el valor p.
  valor_p <- calcular_valor_p(distribucion, observado, repeticiones,
                              alternative)
  
  cat("Valor p:", valor_p, "\n\n")
}

#################### Funciones para boostraping  ##################


# Función que realiza remuestreo con boostraping usando la función sample
boostraping <- function(x){
  instancia <- 1:552
  
  datos_rem_venezuela <- sample(datos_filtrados_venezuela$cantidad_habitantes, 184, replace = TRUE)
  label_venezuela <- c(rep("Venezuela",184))
  df_venezuela <- data.frame(nacionalidad = label_venezuela,cantidad_habitantes= datos_rem_venezuela)
  
  datos_rem_peru <- sample(datos_filtrados_peruanos$cantidad_habitantes, 184, replace = TRUE)
  label_peru <- c(rep("Perú",184))
  df_peru <- data.frame(nacionalidad = label_peru,cantidad_habitantes= datos_rem_peru)
  
  datos_rem_haiti <- sample(datos_filtrados_haitiano$cantidad_habitantes, 184, replace = TRUE)
  label_haiti <- c(rep("Haití",184))
  df_haiti <- data.frame(nacionalidad = label_haiti,cantidad_habitantes= datos_rem_haiti)
  
  return(rbind(df_venezuela,df_peru,df_haiti) %>% cbind(instancia))
}

# Funcion que permite obtener el estadistico F
obtenerF<- function(p) {
  
  suppressMessages(suppressWarnings(anova <- ezANOVA(p,
                   dv = cantidad_habitantes,
                   between = nacionalidad,
                   wid = instancia,
                   return_aov = TRUE)))
  return(anova[["ANOVA"]][["F"]])
}


# Función que permite calcular la distribución de medias para hacer una análisis de post-hoc
diferenciaDeMedias <- function(remuestreos, pais1, pais2, B){
  lista <- c()
  for(i in 1:B){
    datos <- as.data.frame(remuestreos[i])
    nacionalidad1 <- datos %>% filter(nacionalidad == pais1)
    nacionalidad2 <- datos %>% filter(nacionalidad == pais2)
    dif <- mean(nacionalidad1$cantidad_habitantes) - mean(nacionalidad2$cantidad_habitantes)
    lista <- c(lista, dif)
  }
  return(lista)
}

##########################################################################################






# Se carga el archivo de datos CSV
datos <- read.csv2(file.choose(new = FALSE))

################################################################################
# 1. Propongan una pregunta de investigación original, que involucre la
# comparación de las medias de dos grupos independientes (más abajo se dan unos
# ejemplos). Fijando una semilla propia, seleccionen una muestra aleatoria de
# hogares (250 < n < 500) y respondan la pregunta propuesta utilizando una
# simulación MonteCarlo.
################################################################################

# Pregunta de investigación:
#  La pregunta formulada que se busca estudiar es la siguiente ¿Las personas no
#  profesionales (sin título universitario) tienen en promedio una cantidad 
#  mayor de hijos que las personas profesionales (con título universitario)?

#  Hipótesis:
#  H0: No existe diferencia entre las medias de la cantidad de hijos entre 
#     personas no profesionales y personas profesionales.
#  Ha: La media de hijos de las personas no profesionales es mayor a la media
#      de hijos de las personas profesionales.

# Matemáticamente:
#  Sea: µA = Media de hijos de personas no profesionales
#       µB = Media de hijos de personas profesionales.
# H0: µA- µB = 0
# Ha: µA- µB > 0

# Debido a que estamos evaluando promedios de dos poblaciones independientes,
#el estadístico de interés a remuestrear será la media.

# Se fija la seed
set.seed(160)
# Se consiguen 300 datos del dataset
datosMuestra <- sample_n(datos, 300)
# Se filtran datos
datos_filtrados <- datosMuestra %>% select(e6a,s4)
datos_filtrados <- rename(datos_filtrados, nivel_educacional = e6a)
datos_filtrados <- rename(datos_filtrados, cantidad_hijos = s4)
# Se ignoran los datos de no sabe o no responde
datos_filtrados <- datos_filtrados %>% filter(nivel_educacional != "No sabe/no responde")
# Se transforman usando factor a No profesional o Profesional
datos_filtrados$nivel_educacional <- factor(datos_filtrados$nivel_educacional)
levels(datos_filtrados$nivel_educacional) <- c("No profesional",
                                               "No profesional",
                                               "No profesional",
                                               "No profesional",
                                               "No profesional",
                                               "No profesional",
                                               "No profesional",
                                               "Profesional",
                                               "Profesional",
                                               "No profesional",
                                               "No profesional",
                                               "Profesional",
                                               "No profesional",
                                               "No profesional",
                                               "No profesional",
                                               "No profesional",
                                               "No profesional"
)

# Se seleccionan los datos de los profesionales y no profesionales por separado
datos_profesionales <- datos_filtrados %>% filter(nivel_educacional == "Profesional")
datos_noProfesionales <- datos_filtrados %>% filter(nivel_educacional == "No profesional")

# Se fija la cantidad de repeticiones
R <- 5999
# Se vuelve a fijar la seed
set.seed(160)
# Se efectua la simulación de Monte carlo
contrastar_hipotesis_permutaciones(datos_noProfesionales$cantidad_hijos, 
                                   datos_profesionales$cantidad_hijos,
                                   repeticiones = R,
                                   FUN = mean,
                                   alternative = "greater",
                                   plot = TRUE,
                                   color = "blue",
                                   fill = "blue")

# Tomando en consideración el p valor obtenido 0.0027 menor al nivel de 
# significación alfa=0.05 entonces se rechaza la hipótesis nula en favor de la
# hipótesis alternativa, incluso es menor a si fijáramos un valor estricto de
# 0.01 al nivel de significación. Es posible concluir entonces, con 99% de
# confianza, que la media de hijos que tienen las personas no profesionales
# (sin título universitario) es mayor a la media de hijos de las personas
# profesionales (con título universitario).


################################################################################
# 2. Propongan una pregunta de investigación original, que involucre la
# comparación de las medias de más de dos grupos independientes (más abajo se
# dan unos ejemplos). Fijando una semilla distinta a la anterior, seleccionen 
# una muestra aleatoria de hogares (400 < n < 600) y respondan la pregunta
# propuesta utilizando bootstrapping. Solo por ejercicio académico, aplique un
# análisis post-hoc con bootstrapping aunque este no sea necesario.
################################################################################

# Pregunta de investigación:
#  La pregunta formulada que se busca estudiar es la siguiente ¿Los inmigrantes 
#  peruanos, haitianos y venezolanos tienen en promedio una cantidad 
#  igual de habitantes en el hogar?


#  Hipótesis:
#  H0: No existe diferencia entre las medias de habitantes del hogar
#      entre las muestras de los inmigrantes peruanos, haitianos y venezolanos.
#  Ha: Al menos una de las medias de habitantes del hogar es diferente
#      entre las muestras de los inmigrantes peruanos, haitianos y venezolanos.

# El estadístico de interés para la prueba ómnibus que se usara para el 
# remuestreo mediante bootstraping es el estadístico F, se ha elegido este 
# estadístico porque la única forma conocida para poder inferir respecto a las
# medias iguales o mayores a 3 grupos es el procedimiento de ANOVA, el cual
# ocupa el estadístico F para efectuar la inferencia.

#Se filtran los datos
datos_filtrados_ext <- datos %>%select(r1a.cod,numper)
datos_filtrados_ext <- rename(datos_filtrados_ext, nacionalidad = r1a.cod)
datos_filtrados_ext <- rename(datos_filtrados_ext, cantidad_habitantes = numper)
datos_filtrados_ext <- datos_filtrados_ext %>%filter(nacionalidad %in% c("Perú","Haití","Venezuela"))
#Quita filas con NA
datos_filtrados_ext <- datos_filtrados_ext %>% filter(!is.na(datos_filtrados_ext$cantidad_habitantes))
# Se fija la seed
set.seed(777)
# Se consiguen 552 datos del dataset
n = 552
datos_filtrados_ext <- sample_n(datos_filtrados_ext, n)

#Se filtra según la nacionalidad
datos_filtrados_peruanos <- datos_filtrados_ext %>%filter(nacionalidad =="Perú")
datos_filtrados_haitiano <- datos_filtrados_ext %>%filter(nacionalidad =="Haití")
datos_filtrados_venezuela <- datos_filtrados_ext %>%filter(nacionalidad =="Venezuela")

# Se agrega la instancia a cada dato
instancia <- factor(1:n)
datos_filtrados_ext <- datos_filtrados_ext %>% mutate(instancia = instancia)

# Se obtiene el valor correspondiente al estadístico F entregado por ANOVA para
# la muestra inicial
anovaInicial <- ezANOVA(datos_filtrados_ext, dv = cantidad_habitantes, between = nacionalidad, 
                          wid = instancia, return_aov = FALSE)
print(anovaInicial)
valor_F_inicial <- anovaInicial$ANOVA$F

# Se establece la cantidad de muestras a generar
B <- 199

# Se vuelve a fijar la seed
set.seed(777)

# Se generan los datos usando boostrap, aplicando remuestreo
datosGenerados  <- lapply(1:B, boostraping)
# Se genera la distribución de estadísticos F
distribucionesF <- sapply(datosGenerados,obtenerF)

# Obtener valor p.
p <- (sum(abs(distribucionesF) > abs(valor_F_inicial)) + 1) / (B + 1)
cat("Valor p ómnibus =", p, "\n\n")

# El valor p obtenido de 0.705 es mayor al nivel de significación alfa 0.05 
# entonces se falla en rechazar la hipótesis nula H0. Es posible concluir con 95% de 
# confianza que no existe diferencia entre las medias de habitantes del hogar entre
# los inmigrantes peruanos, haitianos y venezolanos que viven en Chile.


######################## Procedimiento Post-hoc ####################

# El estadístico de interés a remuestrear para el análisis post-hoc es la media. 
# En específico la media de los datos conseguidos a través del remuestreo, esto
# debido a que permite saber la diferencia entre cada uno de los grupos y 
# así obtener más información, contradicciones o comprobar los resultados
# de la prueba ómnibus.


# Se calcula la diferencia de medias para los pares de muestras iniciales
diferenciaInicial_ph <- mean(datos_filtrados_peruanos$cantidad_habitantes) - mean(datos_filtrados_haitiano$cantidad_habitantes)
diferenciaInicial_pv <- mean(datos_filtrados_peruanos$cantidad_habitantes) - mean(datos_filtrados_venezuela$cantidad_habitantes)
diferenciaInicial_vh <- mean(datos_filtrados_venezuela$cantidad_habitantes) - mean(datos_filtrados_haitiano$cantidad_habitantes)

# Se calcula la distribución para la diferencia de medias entre las muestras
diferencia_ph <- diferenciaDeMedias(datosGenerados, "Perú", "Haití", B)
diferencia_pv <- diferenciaDeMedias(datosGenerados, "Perú", "Venezuela", B)
diferencia_vh <- diferenciaDeMedias(datosGenerados, "Venezuela", "Haití", B)

# Obtener valores p.
numA <- sum(abs(diferencia_ph) > abs(diferenciaInicial_ph)) + 1
denA <- B + 1
p_peruano_haitiano <- numA / denA

numB <- sum(abs(diferencia_pv) > abs(diferenciaInicial_pv)) + 1
denB <- B + 1
p_peruano_venezolano <- numB / denB

numC <- sum(abs(diferencia_vh) > abs(diferenciaInicial_vh)) + 1
denC <- B + 1
p_venezolano_haitiano <- numC / denC


cat("\n\n")
cat("Análisis post-hoc para la diferencia de las medias\n")
cat("---------------------------------------------------------\n")
cat("Valores p:\n")

cat(sprintf("Peruano - Haitiano: %.3f\n", p_peruano_haitiano))
cat(sprintf("Peruano - Venezolano: %.3f\n", p_peruano_venezolano))
cat(sprintf("Venezolano - Haitiano: %.3f\n", p_venezolano_haitiano))


#Peruano - Haitiano: p = 0.480
#Peruano - Venezolano: p = 0.520
#Venezolano - Haitiano: p = 0.515

# Observando los valores p obtenidos del análisis post-hoc, es posible
# observar que para todos los grupos es mayor al nivel de significación 
# alfa = 0.05, esto significa que se puede afirmar con 95% de confianza 
# que no existe diferencia significativa entre la media de los grupos.
# Este resultado confirma los resultados de la prueba ómnibus que arrojo
# que no existía diferencia significativa entre las medias de los grupos.