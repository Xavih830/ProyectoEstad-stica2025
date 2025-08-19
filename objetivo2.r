#Uso de agua en agricultura

#Integrantes:
# CANDO LEON JUAN ANDRES
# GONZALEZ ZAMBRANO VICTOR FELIX
# MAGALLANES DOMINGUEZ KELVIN ROGER
# CAMACHO GALARZA XAVIER HOMERO

library(ggplot2)
library(dplyr)
agua <- read.csv("EFICIENCIA_AGUA.csv")

#Objetivo2: Contrastar la eficiencia en el uso del agua, 
# medida como rendimiento por volumen de agua, entre los 
# distintos métodos de riego (gravedad, aspersión y goteo),
# con el objetivo de identificar cuál método presenta un
# desempeño significativamente superior en la producción 
# agrícola de la región Costa del Ecuador.

#Primero, mostraremos un gráfico correspondiente para 
# cada variable de este objetivo.

#Eficiencia agua
efa <- agua$eficiencia_agua
hist(efa, main = "Histograma de la eficiencia en el uso del agua 
     (toneladas/m³) en una parcela de la costa\necuatoriana",
     ylab="Frecuencia", xlab="Intervalo de eficiencia del agua",
     breaks=seq(0,12,by=1), right=F)
boxplot(efa)
efaInter <- seq(0, 11, by= 1)
efaRecap <- cut(efa, breaks= efaInter, right=F)
efaFreq <- table(efaRecap)
print(efaFreq)

#Métodos de riego
mr <- agua$metodo_riego
tipoRiego <- factor(mr)
frecuenciaRiego <- table(tipoRiego)
head(frecuenciaRiego)
barplot(frecuenciaRiego)

#Eficiencia del agua para riego por aspersión
efaAsp <- agua %>%
  filter(metodo_riego == "aspersión") %>%
  select(eficiencia_agua) %>%
  unlist()
muA <- mean(efaAsp)
print(muA)
q3A <- quantile(efaAsp, 0.75)
print(q3A)
q1A <- quantile(efaAsp, 0.25)
print(q1A)
RGIa <- q3A - q1A
print(RGIa)

#Eficiencia del agua para riego por goteo
efaGot <- agua %>%
  filter(metodo_riego == "goteo") %>%
  select(eficiencia_agua) %>%
  unlist()
muGo <- mean(efaGot)
print(muGo)
q3Got <- quantile(efaGot, 0.75)
print(q3Got)
q1Got <- quantile(efaGot, 0.25)
print(q1Got)
RGIgot <- q3Got - q1Got
print(RGIgot)

#Eficiencia del agua para riego por gravedad
efaGra <- agua %>%
  filter(metodo_riego == "gravedad") %>%
  select(eficiencia_agua) %>%
  unlist()
muGr <- mean(efaGra)
print(muGr)
q3Gr <- quantile(efaGra, 0.75)
print(q3Gr)
q1Gr <- quantile(efaGra, 0.25)
print(q1Gr)
RGIgr <- q3Gr - q1Gr
print(RGIgr)

#Diagrama de caja para comparar las eficiencias por método de riego
boxplot(efaAsp, efaGot, efaGra)

#Prueba de normalidad para eficiencia por aspersión
qqnorm(efaAsp)
qqline(efaAsp)

#Prueba de normalidad para eficiencia por goteo
qqnorm(efaGot)
qqline(efaGot)

#Prueba de normalidad para eficiencia por gravedad
qqnorm(efaGra)
qqline(efaGra)

#Para ello se implementará ANOVA
#H0: muA = muGo = muGr
#Ha: Al menos una media difiere
rendimiento <- c(efaAsp, efaGot, efaGra)
tratamiento <- factor(c(rep("Aspersión", 712),rep("Goteo",770),
                        rep("Gravedad",752)))
anova <- data.frame(rendimiento, tratamiento)
efaMr <- aov(rendimiento ~ tratamiento, data=anova)
summary(efaMr)

#Al valor p ser menor que el 5% se rechaza la H0,
# al menos una media difiere, pero cuál
#Eso veremos
TukeyHSD(efaMr)

#Como se puede apreciar, solo hay una diferencia, ella está entre
# la eficiencia media del método de riego por goteo y aquella del
# método de riego por gravedad debido a que su valor p es menor a 0.05
#Por lo que goteo difiere del resto

# Verificación de supuestos (residuos)
par(mfrow = c(2, 2))
plot(efaMr)