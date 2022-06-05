# Ejercicio práctico N°12
# Grupo N°2
# Integrantes:
# Christofer Rodriguez - Christian Méndez  - Israel Arias

#Importación de paquetes
if(!require(WRS2)){
  install.packages("WRS2",dependencies = TRUE)
  require(WRS2)
}

if(!require(tidyverse)){
  install.packages("tidyverse",dependencies = TRUE)
  require(tidyverse)
}

if(!require(ggpubr)){
  install.packages("ggpubr",dependencies = TRUE)
  require(ggpubr)
}

###############################################################################
# 1. En el trabajo de título de un estudiante del DIINF se reportan los
# siguientes tiempos de ejecución (en milisegundos) medidos para dos versiones
# de un algoritmo genético para resolver instancias del problema
# del vendedor viajero disponibles en repositorios públicos. ¿Es uno de los
# algoritmos más rápido que el otro?
###############################################################################

# Ingreso de datos
tiempo_A <- c(210041,783108,834565,70599,8576989,251843,4428151,48667,48705,885722)
tiempo_B <- c(2830464,180141,994830,6684497,35974,5743260,4629726,48408,2196277,92932)
tiempo <- c(tiempo_A,tiempo_B)
algoritmo <- c(rep("A", length(tiempo_A)), rep("B", length(tiempo_B)))
datos <- data.frame(tiempo, algoritmo)


# Se comprueba la normalidad
g <- ggqqplot(datos, x = "tiempo", facet.by = "algoritmo",
              palette = c("blue", "red"), color = "algoritmo")

print(g)

# Observando los gráficos Q-Q generados es posible observar la presencia
# de datos atípicos en el algoritmo A, lo que indica que los datos no se
# distribuyen normalmente, por lo que se requiere aplicar una
# transformación a los datos.


# Transformación logaritmica a los datos
tiempo_A <- log(tiempo_A,10)
tiempo_B <- log(tiempo_B,10)
tiempo <- c(tiempo_A,tiempo_B)
datos <- data.frame(tiempo, algoritmo)
# Se comprueba la normalidad con los datos transformados
g <- ggqqplot(datos, x = "tiempo", facet.by = "algoritmo",
              palette = c("blue", "red"), color = "algoritmo")

print(g)

# Visualizando los datos transformados a través de un gráfico Q-Q, se 
# comprueba que no hay datos atípicos, por lo cual se puede afirmar que los 
# datos se distribuyen de una forma similar a la normal.

# Se aplica una prueba t para muestras independientes.

# Hipótesis
# H0: no hay diferencia entre las medias geométricas entre los tiempos del algoritmo A y B.
# HA: hay diferencia entre las medias geométricas entre los tiempos del algoritmo A y B.


# Establecer nivel de significación.
alfa <- 0.05
# Aplicar la prueba t para dos muestras independientes .
prueba <- t.test (x = tiempo_A,
                      y = tiempo_B,
                      paired = FALSE,
                      alternative = "two.sided",
                      mu = 0,
                      conf.level = 1 - alfa )
print(prueba)

# Los intervalos de confianza revirtiendo la transformación logarítmica.
inferior <- 10 ^ prueba$conf.int[1]
superior <- 10 ^ prueba$conf.int[2]
cat("El intervalo de confianza se encuentra entre:",inferior," y ",superior)
# Considerando el p-valor igual a 0.5634 mayor al nivel de significación 0.05
# se falla en rechazar la hipótesis nula. Es posible concluir con 95% de
# confianza que no existe una diferencia entre los tiempos de
# los algoritmos A y B.

###############################################################################
# 2. Analice la primera pregunta abordada en el ejercicio práctico 11, con
# los mismos datos, utilizando un método robusto adecuado
###############################################################################

# Se carga el archivo de datos CSV
dataSet <- read.csv2(file.choose(new = FALSE))

# Pregunta de investigación:
#  La pregunta formulada que se busca estudiar es la siguiente ¿Las personas no
#  profesionales (sin título universitario) tienen en promedio una cantidad 
#  mayor de hijos que las personas profesionales (con título universitario)?

# Se fija la seed
set.seed(160)
# Se consiguen 300 datos del dataset
datos <- sample_n(dataSet, 300)
# Se filtran datos
datos_filtrados <- datos %>% select(e6a,s4)
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

g3 <- ggqqplot(datos_filtrados, x = "cantidad_hijos", facet.by = "nivel_educacional",
              palette = c("blue", "red"), color = "nivel_educacional")

print(g3)

# Debido a que se tiene una gran dispersión de los datos, el transformar a 
# una media truncada o winzorizada no eliminará la gran cantidad de datos 
# atípicos, entonces con el fin que la prueba sea representativa el estadístico 
# que se usará para inferir será la mediana. 


#  Hipótesis:
#  H0: No existe diferencia entre las medianas de la cantidad de hijos entre 
#     personas no profesionales y personas profesionales.
#  Ha: Si existe diferencia entre las medianas de la cantidad de hijos entre 
#     personas no profesionales y personas profesionales.

#Construcción del dataframe
datos <- rbind(datos_profesionales,datos_noProfesionales)
# Establecer nivel de significación y cantidad de muestras a generar
# con bootstrapping .
alfa <- 0.05
bootstrap <- 999

# Aplicar prueba con la mediana
set.seed (135)
prueba_mediana <- pb2gen ( cantidad_hijos ~ nivel_educacional ,
                              data = datos,
                              est = "median",
                              nboot = bootstrap )
cat (" Resultado al usar la mediana como estimador \n\n")
print (prueba_mediana)

# Considerando el p-valor igual a 0.01 menor al nivel de significación 0.05
# se rechaza la hipótesis nula en favor de la alternativa. Es posible concluir con 95% de
# confianza que existe una diferencia entre las medianas de la cantidad de hijos entre
# profesionales y no profesionales.
# Existe una diferencia entre la cantidad de hijos que tienen personas no profesionales
# y personas profesionales.



###############################################################################
# 3. Analice la segunda pregunta abordada en el ejercicio práctico 11, 
# con los mismos datos, utilizando un método robusto adecuado
###############################################################################


# Pregunta de investigación:
#  La pregunta formulada que se busca estudiar es la siguiente ¿Los inmigrantes 
#  peruanos, haitianos y venezolanos tienen en promedio una cantidad 
#  igual de habitantes en el hogar?


#Se filtran los datos
datos_filtrados_ext <- dataSet %>%select(r1a.cod,numper)
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

g4 <- ggqqplot(datos_filtrados_ext, x = "cantidad_habitantes", facet.by = "nacionalidad",
               palette = c("blue", "red","pink"), color = "nacionalidad")

print(g4)

# Dado que los datos no cumplen una distribución cercana a la normal, 
# se hace necesario aplicar una prueba de método robusto.

# Debido a que tenemos tres muestras, para poder inferir sobre las medias
# de estas muestras es que es necesario un procedimiento que permita
# trabajar sobre múltiples grupos como ANOVA, por esto se elige realizar
# la prueba robusta de ANOVA con medias truncadas disponible
# en R con la función t1way().

#  Hipótesis:
#  H0: No existe diferencia entre las medias truncadas de habitantes del hogar
#      entre las muestras de los inmigrantes peruanos, haitianos y venezolanos.
#  Ha: Al menos una de las medias truncadas de habitantes del hogar es diferente
#      entre las muestras de los inmigrantes peruanos, haitianos y venezolanos.


alfa <- 0.05
gamma <- 0.2

medias_truncadas <- t1way ( cantidad_habitantes ~ nacionalidad , data = datos_filtrados_ext , tr = gamma ,
                              alpha = alfa )

print(medias_truncadas)

# Considerando el p-valor igual a 0.01386 menor al nivel de significación 0.05
# se rechaza la hipótesis nula en favor de la alternativa. Es posible concluir con 95% de
# confianza que existe una diferencia entre al menos una de las medias truncadas de los 
# inmigrantes peruanos, haitianos y venezolanos. Para estudiar la diferencia existente entre
# cada uno de los grupos, realizaremos una prueba posthoc.

# Se aplica la prueba post-hoc para la prueba robusta de ANOVA

post_hoc <- lincon(cantidad_habitantes ~ nacionalidad , data = datos_filtrados_ext , tr = gamma ,
                       alpha = alfa )
print(post_hoc)


# Al observar los resultados de la prueba de Lincon, es posible observar que los valores obtenidos
# para los inmigrantes peruanos y venezolanos son menores al nivel de significación 0.05, 
# por lo que se puede afirmar con 95% de confianza que para estos grupos de inmigrantes
# existen diferencias significativas entre las medias truncadas de la cantidad de habitantes en el hogar. 
# Es posible destacar que, para haitianos y peruanos, además de haitianos y venezolanos 
# no existe diferencia significativa, ya que el valor obtenido es mayor al nivel de significancia.


# Tomando en consideración las pruebas aplicadas, es posible concluir de la prueba ómnibus
# que existe diferencia en la cantidad de habitantes promedio en el hogar entre los grupos
# estudiados de peruanos, haitianos y venezolanos. Con la prueba post-hoc se puede concluir
# que existe una diferencia significativa entre la cantidad de habitantes promedio de los 
# grupos de inmigrantes peruanos y venezolanos, no existe diferencia significativa en el 
# promedio de habitantes entre los grupos de haitianos y peruanos o de haitianos y venezolanos.