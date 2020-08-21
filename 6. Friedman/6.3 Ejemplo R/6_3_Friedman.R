library(readr)
wine <- read_csv("~/A CURSO NO PARAMETRICA/CLASES/6. Friedman/6.3 Ejemplo R/wine.csv")
View(wine)

attach(wine)
wine_matrix=data.matrix(wine[,2:6], rownames.force = NA)

#Friedman Test:
friedman.test(wine_matrix)
#p-valor<0.05: Rechazamos H0: Hay diferencias significativas entre los grupos.


#Modificando el dataset:
mi_df <- data.frame(
  "rating" = c(wine$pinecreek,
               wine$saintjude,
               wine$northnapa,
               wine$sevenwinds,
               wine$eternalvalley))

mi_df

wt=c(rep("pinecreek",times=10), rep("saintjude",times=10),
     rep("northnapa",times=10),rep("sevenwinds",times=10),
     rep("eternalvalley",times=10))

mi_df=cbind(mi_df, WineType=wt)

#Comparaciones a pares de grupos:
#Corrección de Bonferroni 
anew=0.05/10
anew
attach(mi_df)
pairwise.wilcox.test(rating, WineType, p.adj = "bonf")





