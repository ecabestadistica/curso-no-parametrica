library(readr)
boxplot2groups <- read_csv("~/A CURSO NO PARAMETRICA/CLASES/8. Gráficos/8.3 Boxplot Ejemplos R/boxplot2groups.csv")
View(boxplot2groups)

library(readr)
boxplot4groups <- read_csv("~/A CURSO NO PARAMETRICA/CLASES/8. Gráficos/8.3 Boxplot Ejemplos R/boxplot4groups.csv")
View(boxplot4groups)

library(readr)
boxplotrep <- read_csv("~/A CURSO NO PARAMETRICA/CLASES/8. Gráficos/8.3 Boxplot Ejemplos R/boxplotrep.csv")
View(boxplotrep)

#Boxplot dos grupos:
boxplot(IQ~School,
        data=boxplot2groups,
        main="Different boxplots for each type of School",
        xlab="School",
        ylab="IQ",
        col="green",
        border="brown"
)

#Boxplot 4 grupos:
boxplot(rating~dose,
        data=boxplot4groups,
        main="Different boxplots for each type of Dose",
        xlab="Dose",
        ylab="Rating",
        col=c("orange", "yellow","green","blue"),
        border="brown"
)


#Boxplot medidas repetidas:
boxplot(rating~brand,
        data=boxplotrep,
        main="Different boxplots for each type of Wine Brand",
        xlab="Wine Brand",
        ylab="Rating",
        border="brown"
)

#Con ggplot
# Convertir la variable de numerica a factor 
boxplot4groups$dose <- as.factor(boxplot4groups$dose)
head(boxplot4groups)

library(ggplot2)
# Box plot basico
p <- ggplot(boxplot4groups, aes(x=dose, y=rating)) + 
  geom_boxplot()
p

# Rotar box plot
p + coord_flip()

# Cambiar el color de fuera segun los grupos
p<-ggplot(boxplot4groups, aes(x=dose, y=rating, color=dose)) +
  geom_boxplot()
p

#Color de dentro
#Uno solo
ggplot(boxplot4groups, aes(x=dose, y=rating)) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic()
#Varios
p<-ggplot(boxplot4groups, aes(x=dose, y=rating, fill=dose)) +
  geom_boxplot()
p




