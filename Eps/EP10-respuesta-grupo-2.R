# Ejercicio práctico N°10
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


# Se carga el archivo de datos CSV
datos <- read.csv2(file.choose(new = FALSE))

###############################################################################
# 1. ¿Existe diferencia entre la puntuación obtenida por los envases diseñados
# por DisenoColor y KoolDesign?
###############################################################################

# Se filtran los datos por Diseño
datos_disenoColor <- datos %>% filter(Diseno == "DisenoColor")
datos_koolDesign <- datos %>% filter(Diseno == "KoolDesign")

# En primer lugar se comprueba la normalidad de los datos
normalidad_disenoColor <- shapiro.test(datos_disenoColor$Puntaje)
print(normalidad_disenoColor)

normalidad_koolDesign <- shapiro.test(datos_koolDesign$Puntaje)
print(normalidad_koolDesign)

# En primer lugar, cabe destacar que se cumple la condición de independencia
# para una prueba T al ser datos ligados a un estudio donde los participantes
# fueron elegidos aleatoriamente, sin embargo, las muestras no tienen una 
# distribución similar a la normal como demuestran las pruebas de Shapiro. 

# Además, no es posible asegurar que la escala de Likert tenga igual intervalo 
# para cada participante, por lo que no es posible aplicar una prueba T, es por
# esto que se decide ocupar su versión no paramétrica Wilcoxon-Mann-Whitney.
# La prueba de Wilcoxon-Mann-Whitney en su versión de suma de rangos tiene dos
# condiciones:

# 1.	Las observaciones de ambas muestras son independientes
# 2.	La escala de medición debe ser ordinal.
# La primera condición ya fue verificada anteriormente, la segunda condición
# también es cumplida ya que la escala de Likert cumple la ordinalidad. 
# Por lo que es posible la aplicación de esta prueba.

#Hipótesis:
#H0: no hay diferencia en la puntuación obtenida entre los envases diseñados
#   por DisenoColor y KoolDesign.
#Ha: sí hay diferencia en la puntuación obtenida entre los envases diseñados
#    por DisenoColor y KoolDesign.

# Aplicación de la prueba

# Establecer nivel de significación.
alfa <- 0.05

# Hacer la prueba de Mann-Whitney de suma de rangos.
prueba <- wilcox.test(datos_disenoColor$Puntaje, 
                      datos_koolDesign$Puntaje,
                      paired = FALSE,
                      alternative = "two.sided",
                      conf.level = 1 - alfa)
print(prueba)


# Como el p-valor es igual a 0.1072 mayor al nivel de significancia 0.05, 
# entonces se falla en rechazar la hipótesis nula. Es posible concluir con 
# 95% de confianza que no hay diferencias en las puntuaciones obtenidas entre
# los envases diseñados por DisenoColor y KoolDesign.


##############################################################################
# 2. ¿Existe diferencias en las puntuaciones obtenidas para el envase de
# alfajores diseñado por PackPro según la edad de los evaluadores? De ser así,
# ¿cuál(es) grupo(s) de evaluador(es) se diferencia(n) de los demás?
##############################################################################  
datos_packPro <- datos %>% filter(Diseno == "PackPro")
datos_alfajores_packPro <- datos_packPro %>% filter(Producto == "Alfajor")

# Se seleccionan solo los datos a ocupar en la prueba
datos_filtrados <- datos_alfajores_packPro %>% select(Puntaje,Edad)

# Por las características del problema pareciese que la prueba a aplicar para
# responder la pregunta es la prueba ANOVA de una vía para muestras
# independientes, sin embargo, la prueba ANOVA en su primer requisito establece
# que la escala con la que se mide la variable independiente, en este caso el 
# puntaje tiene propiedades de una escala de intervalos iguales. En este caso
# no se cumple debido a que la escala de Likert la cual fue usada para medir el
# puntaje no es una escala de intervalos iguales, un 5 de una persona puede no
# ser el mismo 5 de otra persona, por lo que no es posible la aplicación de
# esta prueba. Por esto se aplica su alternativa no paramétrica que es la
# prueba de Krukal-Wallis, se verifican sus condiciones:

#1.	La variable independiente debe tener a lo menos dos niveles.
#2.	La escala de la variable dependiente debe ser a lo menos ordinal.
#3.	Las observaciones son independientes entre sí.

#La primera condición se cumple, ya que la variable independiente
# presenta 3 niveles: Nino, Joven y Adulto. La segunda condición también se
# cumple, debido a que la escala de Likert si es ordinal como fue verificado
#en la pregunta anterior. Por ultimo las observaciones son independientes entre
# sí, ya que fueron elegidas aleatoriamente en el estudio, como fue mencionado
#anteriormente. 


#Hipótesis:
# H0: El promedio de las puntuaciones obtenidas para el envase de alfajores
# de PackPro es igual para los tres grupos etarios de los evaluadores.
# Ha: El promedio de las puntuaciones obtenidas para el envase de alfajores
# de PackPro es diferente para al menos un grupo etario de los evaluadores.

# Establecer nivel de significación
alfa <- 0.05

# Hacer la prueba de Kruskal-Wallis.
prueba <- kruskal.test(datos_filtrados$Puntaje ~ factor(datos_filtrados$Edad), data = datos_filtrados)

print(prueba)

# Como el p-valor es igual a 2.2e-16 mucho menor al nivel de significancia 0.05
# es que se rechaza la hipótesis nula en favor de la hipótesis alternativa. 
# En conclusión, podemos afirmar con 95% de confianza que el promedio de las
# puntuaciones obtenidas para el envase de alfajores es diferente para al menos
# un grupo etario de los evaluadores.

# Como se rechazó la hipótesis nula en favor de la hipótesis alternativa,
# es posible obtener más información realizando una prueba post-hoc.

# Se realiza la prueba post-hoc de Holm:

post_hoc_holm <- pairwise.wilcox.test(datos_filtrados$Puntaje,
                                 datos_filtrados$Edad,
                                 p.adjust.method = "holm",
                                 paired = FALSE)

print(post_hoc_holm)

# Tomando en consideración los resultados de la prueba post-hoc, es posible
# observar que los valores obtenidos para todas las comparaciones son menores 
# al nivel de significancia 0.05. Es posible concluir que existe diferencia 
# significativa entre todos los grupos etarios que evaluaron el envase, además
# no existe un grupo que tenga una diferencia mayor a las demás, debido a que
# todas arrojaron el mismo valor.

# Tomando en consideración el procedimiento efectuado, es posible concluir con
# un 95% de confianza mediante la prueba ómnibus que si existen diferencias
# significativas entre las puntuaciones obtenidas según la edad de los
# evaluadores. Además, tomando en consideración la prueba post-hoc, 
# se puede afirmar con 95% de confianza que existen diferencias 
# significativas entre todos los grupos para las puntuaciones registradas, 
# presentando una diferencia similar.
