library(readr)
revenuedata <- read_csv("~/A CURSO NO PARAMETRICA/CLASES/5. Wilcoxon/5.3 Ejemplo R/revenuedata.csv")
View(revenuedata)

attach(revenuedata)

#Wilcoxon 2 grupos no-independientes (relacionados o pareados)
# Por defecto a ambos lados:
wilcox.test(Q1,Q2,paired=TRUE)
mean(Q1)
mean(Q2)

median(Q1)
median(Q2)

#Alternativa: mayor que
wilcox.test(Q1,Q2,paired=TRUE,alternative = c("greater"))
