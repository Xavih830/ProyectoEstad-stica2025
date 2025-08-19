#Uso de agua en agricultura

#Integrantes:
# CANDO LEON JUAN ANDRES
# GONZALEZ ZAMBRANO VICTOR FELIX
# MAGALLANES DOMINGUEZ KELVIN ROGER
# CAMACHO GALARZA XAVIER HOMERO

library(ggplot2)
library(dplyr)
agua <- read.csv("EFICIENCIA_AGUA.csv")

#Objetivo4: Verificar si existe independencia entre
# el tipo de suelo (franco/arcilloso/arenoso) y
# el nivel de tecnificación (bajo/medio/alto), 
# para detectar si ciertos suelos reciben más inversión tecnológica
# en parcelas agrícolas de la Costa ecuatoriana. 

#Primero, mostraremos un gráfico correspondiente para 
# cada variable de este objetivo.

#Tipo de suelo
ts <- agua$tipo_suelo
tipoS <- factor(ts)
frecuenciaTs <- table(tipoS)
head(frecuenciaTs)
barplot(frecuenciaTs)

#Nivel de tecnificación
nt <- agua$nivel_tecnificacion
nivelTec <- factor(nt)
frecuenciaNt <- table(nivelTec)
head(frecuenciaNt)
barplot(frecuenciaNt)

#Gráfica para apreciar el tipo de suelo con su nivel de tecnificación
tsNt <- data.frame(ts, nt)
ggplot(data = tsNt, aes(x = agua$tipo_suelo, fill = agua$nivel_tecnificacion)) +
  geom_bar() +
  coord_flip()

#Para ello se implementará tabla de contingencia con su respectiva
# prueba chi-cuadrado
#H0: El tipo de suelo es independiente del nivel de tecnificación
# de la parcela
#Ha: No es cierto H0
chi <- table(ts, nt)
head(chi)
addmargins(chi)
chi2 <- round(prop.table(chi), 2)
chi2
addmargins(chi2)
chisq.test(chi)

#Como el valor p no es menor a 0.05, entonces no se rechaza H0