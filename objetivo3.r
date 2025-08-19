#Uso de agua en agricultura

#Integrantes:
# CANDO LEON JUAN ANDRES
# GONZALEZ ZAMBRANO VICTOR FELIX
# MAGALLANES DOMINGUEZ KELVIN ROGER
# CAMACHO GALARZA XAVIER HOMERO

library(UsingR)
library(ggplot2)
library(dplyr)
agua <- read.csv("EFICIENCIA_AGUA.csv")
agua <- agua %>% 
  mutate(eficiencia_escalada = eficiencia_agua * 1000)

#Objetivo3: Estimar la relación entre la eficiencia del agua (ton/m³)
# y otros dos factores (precipitación (mm) y área cultivada (ha)), 
# para cuantificar su impacto combinado en la productividad de 
# parcelas agrícolas de la Costa ecuatoriana.

#Como no existe la variable precipitación en el csv, se la omitirá

#Primero, mostraremos un gráfico correspondiente para 
# cada variable de este objetivo

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

#Área cultivada o densidad de siembra
ac <- agua$densidad_siembra
hist(ac, main = "Histograma del área cultivada (ha) en una\nparcela de
     la costa ecuatoriana",
     ylab="Frecuencia", xlab="Intervalo del área cultivada (ha)",right=F)
boxplot(ac)
acInter <- seq(1000, 8000, by= 500)
acRecap <- cut(ac, breaks= acInter, right=F)
acFreq <- table(acRecap)
print(acFreq)

#Prueba de normalidad para la eficiencia del agua
qqnorm(efa)
qqline(efa)

#Prueba de normalidad para el área cultivada
qqnorm(ac)
qqline(ac)

#Para ello se implementará una prueba de regresión lineal simple
# Y = bo + b1 * X + err
#Efa = b0 + b1 * Ac + err

#Gráfico
#Escalaremos la eficiencia para poder apreciar el comportamiento
# entre variables, pero no tiene incidencia alguna en los resultados
agua <- agua %>% 
  mutate(eficiencia_escalada = eficiencia_agua * 1000)

ggplot(data = agua, aes(densidad_siembra, eficiencia_escalada)) +
  geom_point() +
  geom_abline(color = "red", size = 1)

#Modelo
modAE <- lm(formula = eficiencia_agua ~ densidad_siembra, data = agua)
coefs <- coef(modAE)
b0 <- coefs["(Intercept)"]
b1 <- coefs["densidad_siembra"]
summary(modAE)
aovAE <- anova(modAE)

#Efa = 1.905 - 0.00001244*Ac

#Residuos
res <- residuals(modAE)
mean(res)
sd(res)
ks.test(res, y = "pnorm", 0, sd(res))
hist(res)
qqnorm(res)
qqline(res)

#Como se puede apreciar en su histograma la mayor cantidad de residuos
# son negativos, lo que permite ver que el modelo sobreestima al
# momento de predecir

#Ajustado
ajustAE <- fitted(modAE)
hist(ajustAE)
qqnorm(ajustAE)
qqline(ajustAE)

#Predicciones
acP <- c(3108, 3618, 4404, 4019) #Donde sus Efas fueron de
# 1.7647, 1.0901, 3.1258, 1.3322 respectivamente
da <- data.frame(densidad_siembra = acP)
predict(object = modAE, da)
#Con esa muestra de datos se aprecia, nuevamente que el modelo
# sobreestima como error en su predicción, ya que sus residuos son
# negativos en mayor proporción como ha sido mostrado en el
# histograma de residuos

#Graficando el modelo
ggplot(data = agua, aes(densidad_siembra, eficiencia_escalada)) +
  geom_point() +
  geom_abline(color = "red", size = 1) +
  geom_smooth(method = "lm", se = T)

