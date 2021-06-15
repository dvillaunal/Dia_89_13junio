## ---- eval=FALSE, include=TRUE-------------------------------------------------------
## "Protocolo:
## 
##  1. Daniel Felipe Villa Rengifo
## 
##  2. Lenguaje: R
## 
##  3. Tema: ANÁLISIS DISCRIMINANTE LINEAL Y CUADRÁTICO EN R (LDA)
## 
##  4. Fuentes:
##     https://www.r-bloggers.com/2018/11/linear-quadratic-and-regularized-discriminant-analysis/"


## ------------------------------------------------------------------------------------
#En este ejercicio trabajaremos con el set de datos "Auto" del paquete "ISLR" y ajustaremos un modelo de LDA y QDA para predecir si el rendimiento del combustible (gas mileage) de un automóvil es alto o bajo en función del resto de predictores del set de datos.

# Instalar el paquete de labase de datos:
#install.packages("ISLR")

# Exportamos la base de datos: 
Auto <- write.csv(ISLR::Auto,file = "Auto.csv", row.names = F)

# Importar la base datos:
Auto <- read.csv(file = "Auto.csv", header = T, sep = ",", dec = ".")



"Consideraremos como \“alto\” un valor mayor a la mediana. Para ello, crearemos una variable binaria a partir de la variable binaria mpg01 que contenga \“1\” si el valor de mpg está por encima de la mediana, y \“0\” si se encuentra por debajo. Al hacer esto, será importante codificar la variable respuesta como factor. Como en este caso contamos con un predictor con (K = 2), la regresión logística también sería una opción."


## ------------------------------------------------------------------------------------
# Guardar los OUTPUTS en .txt:
sink(file = "OUTPUTS.txt")

# Visualizaciones de la base de datos:
print("# Visualizaciones de la base de datos:")
print(head(Auto, 10))

# Visualizaciones de la clases de la variables:
print("# Visualizaciones de la clases de la variables:")
print(str(Auto))

# Estadisticos Basicos de la base de datos:
print("# Estadisticos Basicos de la base de datos:")
print(summary(Auto))

# Acceder a los nombres de las variables sin del data.frame:
attach(Auto)

# Vector de “0”s (rendimiento bajo) con la misma longitud que la variable mpg
mpg01 <- rep(0, length(mpg))

# Sustitución de “0”s por “1”s (rendimiento alto) si mpg > mediana(mpg)
mpg01[mpg > median(mpg)] <- 1

Auto <-  data.frame(Auto, mpg01)

# Observación de la nueva variable en la base de datos:
print("# Observación de la nueva variable en la base de datos:")
print(head(Auto, 3))

# Correlación entre variables (excluyendo la variable cualitativa “name”)
print("# Correlación entre variables (excluyendo la variable cualitativa “name”)")
print(cor(Auto[, -9], method = "pearson"))


## ---- message=FALSE------------------------------------------------------------------
# Importamos librerias:
library(dplyr)
#install.packages("GGally")
library(GGally)

# Creamos una matriz de correlación:
png(filename = "MatrizCorr.png")

ggpairs(select(Auto, - name),
        lower = list(continuous = "smooth"),
        diag = list(continuous = "bar"), 
        axisLabels = "none", title = "Matriz de Correlación de \"Auto\"")

dev.off()

######### Vamos a Graficar varios funciones necesarios para dar explicaciones: #########

# 1
png(filename = "Cylindersvsmpg01.png")

boxplot(cylinders ~ mpg01, data = Auto, main = "Cylinders vs mpg01")

dev.off()

# 2
png(filename = "Displacementvsmpg01")

boxplot(displacement ~ mpg01, data = Auto, main = "Displacement vs mpg01")

dev.off()

# 3
png(filename = "Horsepowervsmpg01.png")

boxplot(horsepower ~ mpg01, data = Auto, main = "Horsepower vs mpg01")

dev.off()

# 4
png(filename = "Weightvsmpg01.png")

boxplot(weight ~ mpg01, data = Auto, main = "Weight vs mpg01")

dev.off()

# 5
png(filename = "Accelerationvsmpg01.png")

boxplot(acceleration ~ mpg01, data = Auto, main = "Acceleration vs mpg01")

dev.off()

# 6
png(filename = "Yearvsmpg01.png")

boxplot(year ~ mpg01, data = Auto, main = "Year vs mpg01")

dev.off()


# Asociación entre variables cuantitativas más correlacionadas. Se colorean los casos en función del rendimiento
png(filename = "DispercionRend.png")

pairs(x = Auto[, c("displacement", "weight", "horsepower")], 
      col= ifelse(Auto$mpg01==0, "red", "green"), 
      pch = 4,
      main = "Asociación entre varibles Cuantitativas\nEn función del \"rendimiento\"")

dev.off()


## ---- message=FALSE------------------------------------------------------------------
library(ggplot2)
library(gridExtra)
# Representación de distribución en histogramas

# Histograma del desplazamiento:
p1 <- ggplot(data = Auto, aes(x = displacement)) + 
  geom_histogram(position = "identity", 
                 alpha = 0.5, 
                 aes(fill = as.factor(mpg01)))+
  labs(fill = "mpg01")

# Histogramas de Caballos de fuerza:
p2 <- ggplot(data = Auto, aes(x = horsepower))+
  geom_histogram(position = "identity", 
                 alpha = 0.5, 
                 aes(fill = as.factor(mpg01)))+
  labs(fill = "mpg01")

# Histogramas del peso
p3 <- ggplot(data = Auto, aes(x = weight))+
  geom_histogram(position = "identity", 
                 alpha = 0.5,
                 aes(fill = as.factor(mpg01)))+
  labs(fill = "mpg01")

# Presentamos en un mismo "device" los tres histogramas:
png(filename = "Histogramas.png")

grid.arrange(p1, p2, p3)

dev.off()


# Representación tridimensional
#install.packages("scatterplot3d")
library(scatterplot3d)

# Veamos un grafico de dispersión de tres variables:

png(filename = "Scatterplot3Var.png")

scatterplot3d(Auto$displacement, Auto$horsepower, Auto$weight, pch = 19, 
              grid = TRUE, tick.marks = FALSE, angle = 65, 
              xlab = "displacement", ylab = "horsepower", zlab = "weight", 
              color = ifelse(Auto$mpg01 == 0, yes = "red", no = "green3"))
legend("topleft", bty = "n", cex = .6, 
       title = "Rendimiento", c("alto", "bajo"), 
       fill = c("green3", "red"))

dev.off()


## ------------------------------------------------------------------------------------
# Explicación de todos los graficos hechos:

"Las variables que muestran mayor asociación con mpg01 y que parecen ser útiles en separar los automóviles según el rendimiento (y que por tanto podrían ayudar a predecir esta variable respuesta) son: [displacement], [weight], [horsepower] y [cylinders].

Sin embargo, hay que tener en cuenta, según se aprecia en los gráficos, que la clase 0 de menor rendimiento solapa con la clase 1 de mayor rendimiento, lo que puede afectar la capacidad predictiva del modelo."

