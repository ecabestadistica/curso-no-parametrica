library(tidyverse)
library(broom)
theme_set(theme_classic())

library(readr)
notasmatesingles <- read_csv("A CURSO NO PARAMETRICA/CLASES/2. Distribuciones y varianzas de grupos/2.6 Analizando distribuciones en Python/notasmatesingles.csv")
View(notasmatesingles)

plot(notasmatesingles$Matematicas,notasmatesingles$Ingles)

#Modelo de regresion lineal
model <- lm(Ingles ~ Matematicas, data = notasmatesingles)
model
model.diag.metrics <- augment(model)
head(model.diag.metrics)

#Grafico linea de regresion sobre los datos
ggplot(model.diag.metrics, aes(Matematicas, Ingles)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = Matematicas, yend = .fitted), color = "red", size = 0.3)

#Graficos de diagnosis
par(mfrow = c(2, 2))
plot(model)

install.packages("ggfortify")
library(ggfortify)
autoplot(model)

#Normalidad de los residuos
par(mfrow = c(1, 1))
plot(model, 2)

#Residuos
res=model$residuals

#Shapiro-Wilks
x.test <- shapiro.test(res)
x.test
p_value_thresh=0.05

#Definiendo la función que me devuelve si se cumple o no la hipotesis
sw_test_results <- function(x.test,p_value_thresh) {
  if(x.test$p.value > p_value_thresh){
    print('Assumption satisfied')
  } else {
    print('Assumption not satisfied')
    print('Confidence intervals will likely be affected')
    print('Try performing nonlinear transformations on variables')
  }
  
}

#Probando la funcion
sw_test_results(x.test,p_value_thresh)


plotn <- function(x,main="Histograma de frecuencias \ny distribución normal",
                  xlab="X",ylab="Densidad") {
  min <- min(x)
  max <- max(x)
  media <- mean(x)
  dt <- sd(x)
  hist(x,freq=F,main=main,xlab=xlab,ylab=ylab)
  curve(dnorm(x,media,dt), min, max,add = T,col="blue")
}

plotn(res,main="Distribución normal")




