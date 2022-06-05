# Ejercicio práctico N°9
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



# Se carga el archivo de datos CSV
datos <- read.csv2(file.choose(new = FALSE))

####################################################################
# En este momento, los investigadores buscan determinar si existen
# diferencias en el tiempo que tardan los usuarios en formular 
# consultas para problemas con diferente nivel de dificultad 
# en el área de computación.
###################################################################

# Se filtran los datos por el área de computación
datos_area_computacion <- datos %>% filter(area == "Computación")
#Se filtran los datos por dificultad
datos_dificultad_baja <- datos_area_computacion %>% filter(dificultad == "Baja")
datos_dificultad_media <- datos_area_computacion %>% filter(dificultad == "Media")
datos_dificultad_alta <- datos_area_computacion %>% filter(dificultad == "Alta")

# Las condiciones para aplicar ANOVA para muestras correlacionadas son:

# 1. La escala con que se mide la variable dependiente tiene
#     las propiedades de una escala de intervalos iguales.

# R: Al ser la variable dependiente el tiempo, efectivamente
# se cumple que tenga una escala de razón de iguales intervalos. 
# Por lo que se verifica esta condición.

# 2. Las mediciones son independientes al interior de cada grupo.

# R: Las muestras fueron elegidas aleatoriamente al pertenecer
# a un estudio en el cual solo participaron voluntarios, por lo cual
# se cumple la condición.

# 3. Se puede suponer razonablemente que la(s) población(es) 
#   de origen sigue(n) una distribución normal.

# Comprobción de normalidad.
#Se comprueba la normalidad para los problemad de dificultad baja
g <- ggqqplot (datos_dificultad_baja,
                  x = "tiempo",
                  y = "dificultad",
                  color = "blue")

g <- g + facet_wrap (~dificultad )
g <- g + rremove ("x.ticks") + rremove ("x.text")
g <- g + rremove ("y.ticks") + rremove ("y.text")
g <- g + rremove ("axis.title")
print (g)

#Se comprueba la normalidad para los problemas de dificultad media
g <- ggqqplot (datos_dificultad_media,
               x = "tiempo",
               y = "dificultad",
               color = "green")

g <- g + facet_wrap (~dificultad )
g <- g + rremove ("x.ticks") + rremove ("x.text")
g <- g + rremove ("y.ticks") + rremove ("y.text")
g <- g + rremove ("axis.title")
print (g)

#Se comprueba la normalidad para los problemas de dificultad alta
g <- ggqqplot (datos_dificultad_alta,
               x = "tiempo",
               y = "dificultad",
               color = "red")

g <- g + facet_wrap (~dificultad )
g <- g + rremove ("x.ticks") + rremove ("x.text")
g <- g + rremove ("y.ticks") + rremove ("y.text")
g <- g + rremove ("axis.title")
print (g)

# Es posible observar gracias a los gráficos Q-Q que
# se cumple esta condición solo para los problemas con dificultad media,
# para los problemas con dificultad alta y baja  se presenta valores atípicos,
# por lo cual se recomendaría disminuir el nivel de significancia a un alfa = 0.025.


# 4. La matriz de varianzas-covarianzas es esférica
# Esto se confirmará más adelante al aplicar la prueba ezAnova()

#Se formulan las hipótesis:

#Hipótesis:
#H0: El tiempo promedio que toma un participante en formular una consulta
#    en el área de computación es igual para las tres dificultades.
#Ha: El tiempo promedio que toma un participante en formular una consulta
#    en el área de computación es diferente para al menos una de las tres dificultades.

# Se llevan los datos a un solo dataset
instancia <- factor(1:200)
dataTiempos <- data.frame(instancia,
                          datos_dificultad_baja$tiempo,
                          datos_dificultad_media$tiempo,
                          datos_dificultad_alta$tiempo)

dataTiempos <- rename(dataTiempos, Baja = datos_dificultad_baja.tiempo)
dataTiempos <- rename(dataTiempos, Media = datos_dificultad_media.tiempo)
dataTiempos <- rename(dataTiempos, Alta = datos_dificultad_alta.tiempo)


# Llevar data frame a formato largo.
dataTiempos <- dataTiempos %>% pivot_longer (c("Baja", "Media", "Alta"),
                                       names_to = "dificultad",
                                       values_to = "tiempo")

dataTiempos[["dificultad"]] <- factor (dataTiempos[["dificultad"]])


# Se aplica la prueba de ANOVA
prueba <- ezANOVA (
  data = dataTiempos,
  dv = tiempo,
  within = dificultad,
  wid = instancia,
  return_aov = TRUE)

cat("El resultado de la prueba de esfericidad de Mauchly:\n\n")
print(prueba[["Mauchly's Test for Sphericity"]])

cat("El resultado de la prueba de ANOVA:\n\n")
print(prueba[["ANOVA"]])

# Como el valor de p para la prueba de esfericidad de Mauchly es igual a p=0.574
# se cumple la condición N°4 para aplicar la prueba, o sea la matriz de varianza-covarianza es esférica.

# Luego, tomando en consideración los resultados de la prueba ANOVA:
# Como el valor de p = 1.66813e-8 es menor al nivel de significación 0.025 definido anteriormente,
# es incluso menor a si fuésemos estrictos y fijáramos un alfa = 0.01, entonces se rechaza la hipótesis nula
# en favor de la hipótesis alternativa. En conclusión, podemos afirmar con un 99% de confianza que el tiempo promedio que 
# toma un participante en formular una consulta para el área de computación
# es diferente para el menos una de las tres dificultades.

# Como se rechazó la hipótesis nula en favor de la alternativa, es posible obtener
# más información aplicando una prueba post-hoc,
# para este caso es posible determinar que dificultades en especifico son las que tienen diferencia respecto a los tiempos.

# Se fija el mismo nivel de significancia utilizado anteriormente
alfa <- 0.01

# Se aplica el procedimiento post-hoc de Holm
holm <- pairwise.t.test (dataTiempos[["tiempo"]],
                         dataTiempos[["dificultad"]],
                            p.adj = "holm",
                            paired = TRUE,
                            conf.level = 1 - alfa )

print(holm)

# Al observar los resultados de la prueba de Holm, es posible observar que los valores obtenidos
# para las dificultades son menores al nivel de significación 0.01, entre Alta-Baja y 
# Alta-Media por lo que se puede afirmar con 99% de confianza que para estas dificultades
# existen diferencias significativas entre los tiempos promedios que tardan los participantes 
# en formular una consulta. 
# Es posible destacar que para Baja-Media no existe diferencia significativa, ya que el valor
# obtenido es mayor al nivel de significancia.


# Finalmente, para concluir, se puede afirmar con 99% de confianza que existen diferencias entre el tiempo promedio
# que tardan los usuarios en formular una consulta las distintas dificultades del área de computación.
# Además, realizando luego un análisis post-hoc se afirma con 99% de confianza que las diferencias significativas
# existentes son entre las dificultades de Alta-Baja y Alta-Media, por otro lado entre las dificultades Baja-Media no existe una diferencia significativa.