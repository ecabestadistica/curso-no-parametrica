library(ggplot2)
library(readr)
pinetree <- read_csv("~/A CURSO NO PARAMETRICA/CLASES/7. Correlación rho Spearman/7.5 Ejemplo 2 R/pinetree.csv")
View(pinetree)

ggplot(pinetree, aes(x=beetle_density, y=pine_population)) + 
  geom_point(color='#2980B9', size = 4) + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color='#2C3E50')

######################## Normalidad 
library(tidyverse)
library(broom)
theme_set(theme_classic())

#Modelo de regresion lineal
model <- lm(pine_population ~ beetle_density, data = pinetree)
model
model.diag.metrics <- augment(model)
head(model.diag.metrics)

#Grafico linea de regresion sobre los datos
ggplot(model.diag.metrics, aes(beetle_density, pine_population)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = beetle_density, yend = .fitted), color = "red", size = 0.3)

#Graficos de diagnosis
par(mfrow = c(2, 2))
plot(model)


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

#Plot de los residuos
df <- data.frame(
  res
)
# Histograma con density plot
ggplot(df, aes(x=res)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 



#Correlacion Spearman
corr <- cor.test(x=pinetree$beetle_density, y=pinetree$pine_population, method = 'spearman')
corr

p_value_thresh=0.05
if(corr$p.value > p_value_thresh){
  cat('pvalor =',w$p.value, "\n")
  cat('No rechazamos H0: Correlación no significativa.')
} else {
  cat('pvalor =',corr$p.value, "\n")
  cat('Rechazamos H0: Correlación significativa.', "\n")
  cat('corr coef =', corr$estimate)
}

