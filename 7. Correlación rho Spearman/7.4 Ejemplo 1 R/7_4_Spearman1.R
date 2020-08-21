library(ggplot2)
library(readr)
employeeratings <- read_csv("~/A CURSO NO PARAMETRICA/CLASES/7. Correlación rho Spearman/7.4 Ejemplo 1 R/employeeratings.csv")
View(employeeratings)


ggplot(employeeratings, aes(x=Score, y=Rating)) + 
  geom_point(color='#2980B9', size = 4) + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color='#2C3E50')

corr <- cor.test(x=employeeratings$Score, y=employeeratings$Rating, method = 'spearman')
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

