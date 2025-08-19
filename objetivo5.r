#Uso de agua en agricultura

#Integrantes:
# CANDO LEON JUAN ANDRES
# GONZALEZ ZAMBRANO VICTOR FELIX
# MAGALLANES DOMINGUEZ KELVIN ROGER
# CAMACHO GALARZA XAVIER HOMERO

library(ggplot2)
library(dplyr)
agua <- read.csv("EFICIENCIA_AGUA.csv")

#Objetivo5: Comparar el volumen medio de agua (m³) utilizado
# entre los niveles de tecnificación bajo, medio y alto, 
# para determinar si existen diferencias significativas en
# la eficiencia hídrica de parcelas agrícolas en la Costa ecuatoriana. 

#Primero, mostraremos un gráfico correspondiente para 
# cada variable de este objetivo.

#Volumen agua
va <- agua$volumen_agua
hist(va, main = "Histograma del volumen de agua (m3) usado en 
     una parcela de la costa\necuatoriana",
     ylab="Frecuencia", xlab="Intervalo del volumen de agua",right=F)
boxplot(va)
vaInter <- seq(0, 20000, by= 1000)
vaRecap <- cut(va, breaks= vaInter, right=F)
vaFreq <- table(vaRecap)
print(vaFreq)

#Niveles de tecnificación
nt <- agua$nivel_tecnificacion
nivelTec <- factor(nt)
frecuenciaNt <- table(nivelTec)
head(frecuenciaNt)
barplot(frecuenciaNt)

#Volumen de agua usado por nivel alto
vaNa <- agua %>%
  filter(nivel_tecnificacion == "alto") %>%
  select(volumen_agua) %>%
  unlist()
muNa <- mean(vaNa)
print(muNa)

#Volumen de agua usado por nivel medio
vaNm <- agua %>%
  filter(nivel_tecnificacion == "medio") %>%
  select(volumen_agua) %>%
  unlist()
muNm <- mean(vaNm)
print(muNm)

#Volumen de agua usado por nivel bajo
vaNb <- agua %>%
  filter(nivel_tecnificacion == "bajo") %>%
  select(volumen_agua) %>%
  unlist()
muNb <- mean(vaNb)
print(muNb)

#Diagrama de caja para comparar las eficiencias por método de riego
boxplot(vaNa, vaNm, vaNb)

#Prueba de normalidad para volumen de agua usado por nivel alto
qqnorm(vaNa)
qqline(vaNa)

#Prueba de normalidad para Volumen de agua usado por nivel medio
qqnorm(vaNm)
qqline(vaNm)

#Prueba de normalidad para Volumen de agua usado por nivel bajo
qqnorm(vaNb)
qqline(vaNb)

#Para ello se implementará ANOVA
#H0: muNa = muNm = muNb
#Ha: Al menos una media difiere
rendimiento <- c(vaNa, vaNm, vaNb)
tratamiento <- factor(c(rep("Alto", 467),rep("Medio",1100),
                        rep("Bajo",667)))
anova <- data.frame(rendimiento, tratamiento)
vaNt <- aov(rendimiento ~ tratamiento, data=anova)
summary(vaNt)

#Al valor p ser menor que el 5% se rechaza la H0,
# al menos una media difiere, pero cuál
#Eso veremos
TukeyHSD(vaNt)

#Como se puede apreciar, todos los volúmenes medios por nivel de 
# tecnificación difieren entre sí debido a que sus valores p son menores a
# 0.05
#Por su diferencia, el nivel alto es el que usa más volumen de agua
# en promedio

#Verificación de supuestos (residuos)
par(mfrow = c(2, 2))
plot(vaNt)