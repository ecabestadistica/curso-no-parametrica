library(readr)
library(car)

################### 2 grupos #################################
homogeneidad2grupos <- read_csv("~/A CURSO NO PARAMETRICA/CLASES/2. Distribuciones y varianzas de grupos/2.8 Homogeneidad (igualdad) de varianzas Levene Test en R/homogeneidad2grupos.csv")
View(homogeneidad2grupos)

attach(homogeneidad2grupos)
position=as.factor(position)
is.factor(position)

#Levene con la media
leveneTest(salary,position, center="mean", data = homogeneidad2grupos)

#Levene con la mediana
leveneTest(salary,position, center="median", data = homogeneidad2grupos)

detach(homogeneidad2grupos)
rm(homogeneidad2grupos)
rm(position)


################### 3 grupos #################################
homogeneidad3grupos <- read_csv("~/A CURSO NO PARAMETRICA/CLASES/2. Distribuciones y varianzas de grupos/2.8 Homogeneidad (igualdad) de varianzas Levene Test en R/homogeneidad3grupos.csv")
View(homogeneidad3grupos)
attach(homogeneidad3grupos)
homogeneidad3grupos$position=as.factor(homogeneidad3grupos$position)

#Levene con la media
leveneTest(salary,position, center="mean", data = homogeneidad3grupos)

#Levene con la mediana
leveneTest(salary,position, center="median", data = homogeneidad3grupos)
