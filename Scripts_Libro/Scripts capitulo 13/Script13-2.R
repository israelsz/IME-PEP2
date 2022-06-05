library(ggpubr)

# Cargar datos
animal <- c("Mountain beaver", "Cow", "Grey wolf", "Goat", "Guinea pig",
            "Dipliodocus", "Asian elephant", "Donkey", "Horse",
            "Potar monkey", "Cat", "Giraffe", "Gorilla", "Human",
            "African elephant", "Triceratops", "Rhesus monkey", "Kangaroo",
            "Golden hamster", "Mouse", "Rabbit", "Sheep", "Jaguar",
            "Chimpanzee", "Brachiosaurus", "Mole", "Pig")

body_weight <- c(1.35, 465, 36.33, 27.66, 1.04, 11700, 2547, 187.1, 521, 10,
                 3.3, 529, 207, 62, 6654, 9400, 6.8, 35, 0.12, 0.023, 2.5,
                 55.5, 100, 52.16, 87000, 0.122, 192)

brain_weight <- c(465, 423, 119.5, 115, 5.5, 50, 4603, 419, 655, 115, 25.6,
                  680, 406, 1320, 5712, 70, 179, 56, 1, 0.4, 12.1, 175, 157,
                  440, 154.5, 3, 180)

datos <- data.frame(animal, body_weight, brain_weight)

# Aplicar transformación logarítmica
log_cuerpo <- log(body_weight)
log_cerebro <- log(brain_weight)
datos <- data.frame(datos, log_cuerpo, log_cerebro)

# Histogramas para el peso cerebral antes y después de la transformación
# logarítmica.
g3 <- gghistogram(datos, x = "brain_weight", bins = 10,
                  xlab = "Peso del cerebro [g]", ylab = "Frecuencia",
                  color = "red", fill = "red")

g4 <- gghistogram(datos, x = "log_cerebro", bins = 10,
                  xlab = "Peso del cerebro [log(g)]", ylab = "Frecuencia",
                  color = "red", fill = "red")

# Crear una única figura con ambos histogramas.
histograma  <- ggarrange(g3, g4, ncol = 2, nrow = 1)

titulo  <- text_grob("Efecto de la transformación logarítmica",
                     face = "bold", size = 14)

histograma <- annotate_figure(histograma, top = titulo)
print(histograma)

# Gráficos de dispersión para la relación entre peso corporal y peso del
# cerebro, antes y después de aplicar la transformación logarítmica.
g1 <- ggscatter(datos, x = "body_weight", y = "brain_weight",
                color = "red", xlab = "Peso corporal [Kg]",
                ylab = "Peso del cerebro [g]") + rotate_x_text(45)

g2 <- ggscatter(datos, x = "log_cuerpo", y = "log_cerebro",
                color = "red", xlab = "Peso corporal [log(Kg)]",
                ylab = "Peso del cerebro [log(g)]") + rotate_x_text(45)

# Crear una única figura con los gráficos de dispersión.
dispersion  <- ggarrange(g1, g2, ncol = 2, nrow = 1)
texto <- "Relación entre el peso corporal y el peso del cerebro"
titulo  <- text_grob(texto, face = "bold", size = 14)
dispersion <- annotate_figure(dispersion, top = titulo)
print(dispersion)