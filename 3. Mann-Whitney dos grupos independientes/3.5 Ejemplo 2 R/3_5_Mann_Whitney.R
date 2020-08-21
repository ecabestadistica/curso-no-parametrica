library(readr)
salaries <- read_csv("~/A CURSO NO PARAMETRICA/CLASES/3. Mann-Whitney dos grupos independientes/3.5 Ejemplo 2 R/salaries.csv")
View(salaries)

#Mann-Whitney 2 grupos independientes 
#Hay que ver si se cumple la normalidad
grupo1=salaries[salaries$Position==1,]
grupo2=salaries[salaries$Position==2,]


res1=grupo1$Salary-mean(grupo1$Salary)
res2=grupo2$Salary-mean(grupo2$Salary)

#Shapiro-Wilks
x.test1 <- shapiro.test(res1)
x.test1
x.test2 <- shapiro.test(res2)
x.test2
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

#Definiendo la función del grafico de distribucion Normal
plotn <- function(x,main="Histograma de frecuencias \ny distribución normal",
                  xlab="X",ylab="Densidad") {
  min <- min(x)
  max <- max(x)
  media <- mean(x)
  dt <- sd(x)
  hist(x,freq=F,main=main,xlab=xlab,ylab=ylab)
  curve(dnorm(x,media,dt), min, max,add = T,col="blue")
}

#Probando la funcion sobre res1 
sw_test_results(x.test1,p_value_thresh)
plotn(res1,main="Distribución normal")
#Probando la funcion sobre res2 
sw_test_results(x.test2,p_value_thresh)
plotn(res2,main="Distribución normal")

### No se cumple la normalidad para uno de los residuos

### Vamos a  usar la prueba no paramétrica de Mann-Whitney

attach(salaries)
w=wilcox.test(Salary~Position)

p_value_thresh=0.05
if(w$p.value > p_value_thresh){
  cat('pvalor =',w$p.value, "\n")
  cat('No rechazamos H0: No hay diferencias significativas entre los 2 grupos')
} else {
  cat('pvalor =',w$p.value, "\n")
  cat('Rechazamos H0: Hay diferencias significativas entre los 2 grupos')
}
  
w=wilcox.test(Salary~Position,alternative="less")

p_value_thresh=0.05
if(w$p.value > p_value_thresh){
  cat('pvalor =',w$p.value, "\n")
  cat('No rechazamos H0: No hay diferencias significativas entre los 2 grupos')
} else {
  cat('pvalor =',w$p.value, "\n")
  cat('Rechazamos H0: Hay diferencias significativas entre los 2 grupos')
}





  