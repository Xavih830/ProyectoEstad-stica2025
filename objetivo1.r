#Uso de agua en agricultura

#Integrantes:
# CANDO LEON JUAN ANDRES
# GONZALEZ ZAMBRANO VICTOR FELIX
# MAGALLANES DOMINGUEZ KELVIN ROGER
# CAMACHO GALARZA XAVIER HOMERO

library(ggplot2)
library(dplyr)
agua <- read.csv("EFICIENCIA_AGUA.csv")

#Objetivo1: Determinar si el rendimiento, medido en toneladas, del 
# método de fertilización química supera en 0.15 unidades a la 
# orgánica, para conocer qué método genera una mayor producción por 
# parcela en la Costa ecuatoriana.

#Primero, mostraremos un gráfico correspondiente para 
# cada variable de este objetivo.

#Fertilización utilizada
fu <- agua$fertilizacion_utilizada
fuFil <- subset(fu, fu == "orgánica" | fu == "química")
tipoFerti <- factor(fuFil)
frecuenciaFerti <- table(tipoFerti)
head(frecuenciaFerti)
barplot(frecuenciaFerti, main= "Fertilización utilizada", 
        xlab= "Tipo de fertilización", ylab= "Frecuencia", 
        ylim = c(0,800), names.arg= c("Orgánica\n(754)",
        "Química\n(726)"))

#Rendimiento cultivo
ren <- agua$rendimiento_cultivo
hist(ren, main = "Histograma",
     ylab="Frecuencia", xlab="Intervalo de rendimiento de la parcela")
boxplot(ren)
renInter <- seq(0, 36000, by= 2000)
renRecap <- cut(ren, breaks= renInter, right=FALSE)
renFreq <- table(renRecap)
print(renFreq)

qqnorm(ren)
qqline(ren)

#Rendimiento del cultivo para fertilización orgánica
renOrg <- agua %>%
  filter(fertilizacion_utilizada == "orgánica") %>%
  select(rendimiento_cultivo) %>%
  unlist()
muO <- mean(renOrg)
muO
#Rendimiento del cultivo para fertilización química
renQuim <- agua %>%
  filter(fertilizacion_utilizada == "química") %>%
  select(rendimiento_cultivo) %>%
  unlist()
muQ <- mean(renQuim)
muQ
#Diagrama de caja para comparar ambas eficiencias
boxplot(renOrg, renQuim)

#Prueba de normalidad para la eficiencia con fertilización orgánica
qqnorm(renOrg)
qqline(renOrg)

#Prueba de normalidad para la eficiencia con fertilización química
qqnorm(renQuim)
qqline(renQuim)

#Se hará una prueba de hipótesis de dos medias

#Antes de ello se debe de comprobar la igualdad de varianzas
#Prueba de varianzas
var.test(x = renOrg, y = renQuim,
         alternative = "two.sided",
         conf.level = 0.9)

#Por lo tanto, las varianzas son iguales

#H0: muQ - muO >= 0.15
#Ha: muQ - muO < 0.15
t.test(x = renQuim, y = renOrg, 
       alternative = "less",
       mu = 0.15, 
       paired = F, 
       var.equal = T)

#Por lo tanto no se rechaza la hipótesis nula

