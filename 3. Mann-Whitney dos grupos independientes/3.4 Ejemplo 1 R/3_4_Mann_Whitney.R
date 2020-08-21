library(readr)
forceps <- read_csv("~/A CURSO NO PARAMETRICA/CLASES/3. Mann-Whitney dos grupos independientes/3.4 Ejemplo 1 R/forceps.csv")
View(forceps)


#Mann-Whitney 2 grupos independientes
#Una variable dependiente (Resection) y una variable con los factores o grupos (Forceps)

attach(forceps)
w=wilcox.test(Resection~Forceps)

p_value_thresh=0.05

if(w$p.value > p_value_thresh){
  cat('pvalor =',w$p.value, "\n")
  cat('No rechazamos H0: No hay diferencias significativas entre los 2 grupos')
} else {
  cat('pvalor =',w$p.value, "\n")
  cat('Rechazamos H0: Hay diferencias significativas entre los 2 grupos')
}

# Hay diferencias significativas entre los 2 grupos: pero ¿Jumbo extrae mayor tejido? --> Sí:
wilcox.test(Resection~Forceps, alternative = c("less"))
