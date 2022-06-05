# Ejercicio práctico N°8
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
# diferencias en el tiempo que tardan los usuarios
# en formular una consulta para un problema de dificultad difícil 
# en las áreas de psicología, música y matemáticas.
###################################################################

# Se filtran los datos por dificultad
datos_dificultad_alta <- datos %>% filter(dificultad == "Alta")
#Se filtran los datos por categorias
datos_psicologia <- datos_dificultad_alta %>% filter(area == "Psicología")
datos_musica <- datos_dificultad_alta %>% filter(area == "Música")
datos_matematicas <- datos_dificultad_alta %>% filter(area == "Matemáticas")

# Las condiciones para aplicar ANOVA son:
# 1. La escala con que se mide la variable dependiente tiene
#     las propiedades de una escala de intervalos iguales.

# R: Al ser la variable dependiente el tiempo, efectivamente
# se cumple que tenga una escala de razón de iguales intervalos. 
# Por lo que se verifica esta condición.

# 2. Las k muestras son obtenidas de manera aleatoria e 
#    independiente desde la(s) población(es) de origen.

# R: Las muestras fueron elegidas aleatoriamente al pertenecer
# a un estudio en el cual solo participaron voluntarios, por lo cual
# se cumple la condición.

# 3. Se puede suponer razonablemente que la(s) población(es) 
#   de origen sigue(n) una distribución normal.

# Comprobción de normalidad.
#Se comprueba la normalidad para el área de matemáticas
g <- ggqqplot (datos_matematicas,
                  x = "tiempo",
                  y = "area",
                  color = "blue")

g <- g + facet_wrap (~area )
g <- g + rremove ("x.ticks") + rremove ("x.text")
g <- g + rremove ("y.ticks") + rremove ("y.text")
g <- g + rremove ("axis.title")
print (g)

#Se comprueba la normalidad para el área de música
g <- ggqqplot (datos_musica,
               x = "tiempo",
               y = "area",
               color = "green")

g <- g + facet_wrap (~area )
g <- g + rremove ("x.ticks") + rremove ("x.text")
g <- g + rremove ("y.ticks") + rremove ("y.text")
g <- g + rremove ("axis.title")
print (g)

#Se comprueba la normalidad para el área de psicología
g <- ggqqplot (datos_psicologia,
               x = "tiempo",
               y = "area",
               color = "red")

g <- g + facet_wrap (~area )
g <- g + rremove ("x.ticks") + rremove ("x.text")
g <- g + rremove ("y.ticks") + rremove ("y.text")
g <- g + rremove ("axis.title")
print (g)

# Es posible observar gracias a los gráficos Q-Q que
# se cumple esta condición, salvo para el área de psicología,
#en la cual se presenta un valor atípico, por lo cual se
#recomendaría disminuir el nivel de significancia a un alfa = 0.025.


# 4. Las k muestras tienen varianzas aproximadamente iguales.

varianzaMate <- var(datos_matematicas$tiempo)
varianzaMusica <- var(datos_musica$tiempo)
varianzaPsicologia<- var(datos_psicologia$tiempo)
razonMayorMenor <- varianzaPsicologia/varianzaMate
cat("La razón entre las varianzas mayor y menor es: ", razonMayorMenor)
cat("\n")
# La razón es menor a 1.5, por lo tanto se cumple la condición.

#Se formulan las hipótesis:

#Hipótesis:
#H0: El tiempo promedio que toma un participante en formular una consulta es
#   igual para las tres áreas.
#Ha: El tiempo promedio que toma un participante en formular una consulta es
#   diferente para al menos una de las tres áreas.

# Se llevan los datos a un solo dataset
dataTiempos <- data.frame(datos_matematicas$tiempo,datos_musica$tiempo,datos_psicologia$tiempo)
dataTiempos <- rename(dataTiempos, Matemáticas = datos_matematicas.tiempo)
dataTiempos <- rename(dataTiempos, Música = datos_musica.tiempo)
dataTiempos <- rename(dataTiempos, Psicología = datos_psicologia.tiempo)


# Llevar data frame a formato largo.
dataTiempos <- dataTiempos %>% pivot_longer (c("Matemáticas", "Música", "Psicología"),
                                       names_to = "área",
                                       values_to = "tiempo")

dataTiempos[["área"]] <- factor (dataTiempos[["área"]])
dataTiempos[["instancia"]] <- factor (1:nrow (dataTiempos))


# Se aplica la prueba de ANOVA
prueba <- ezANOVA (
  data = dataTiempos,
  dv = tiempo,
  between = área,
  wid = instancia,
  return_aov = TRUE)

print(prueba)

# Como el valor de p = 4.49063e-13 es menor al nivel de significación 0.025 definido anteriormente,
# es incluso menor a si fuésemos estrictos y fijáramos un alfa = 0.01, entonces se rechaza la hipótesis nula
# en favor de la hipótesis alternativa. En conclusión, podemos afirmar con un 99% de confianza que el tiempo promedio que 
# toma un participante en formular una consulta es diferente para el menos una de las tres áreas.


# Como se rechazó la hipótesis nula en favor de la alternativa, es posible obtener más información aplicando una prueba
# post-hoc, para este caso es posible determinar que materias en especifico son las que tienen diferencia respecto a los tiempos.

# Se fija el mismo nivel de significancia utilizado anteriormente
alfa <- 0.01

# Se aplica el procedimiento post-hoc de Holm
holm <- pairwise.t.test (dataTiempos[["tiempo"]],
                         dataTiempos[["área"]],
                            p.adj = "holm",
                            pool.sd = TRUE,
                            paired = FALSE,
                            conf.level = 1 - alfa )

print(holm)

# Al observar los resultados de la prueba de Holm, es posible observar que todos los valores obtenidos
# para todas las áreas son menores al nivel de significación 0.01, por lo que se puede afirmar con 99% de confianza que para todas las áreas
# existen diferencias significativas entre los tiempos promedios que tardan los participantes en formular una consulta. 
# Es posible observar que la mayor diferencia se encuentra entre las áreas de música y psicología.

#Finalmente, para concluir, se puede afirmar con 99% de confianza que existen diferencias entre el tiempo promedio
# que tardan los usuarios en formular una consulta para un problema de dificultad difícil en las áreas de psicología,
# música y matemáticas. Además, realizando luego un análisis post-hoc se afirma con 99% de confianza que las diferencias
# significativas existentes son entre las tres áreas estudiadas.