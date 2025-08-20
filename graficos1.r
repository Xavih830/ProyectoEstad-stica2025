install.packages(c("ggplot2", "dplyr", "ggpubr"))

library(ggplot2)
library(ggpubr)
library(dplyr)

agua <- read.csv("EFICIENCIA_AGUA.csv")

# Objetivo1: Determinar si el rendimiento del método químico supera en 0.15 unidades al orgánico

# ---------------------------------------------------------------
# 1. ANÁLISIS EXPLORATORIO
# ---------------------------------------------------------------

# Gráfico 1: Tipo de fertilización (versión mejorada con ggplot)
fert_plot <- agua %>%
  filter(fertilizacion_utilizada %in% c("orgánica", "química")) %>%
  ggplot(aes(x = fertilizacion_utilizada, fill = fertilizacion_utilizada)) +
  geom_bar(alpha = 0.8) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  scale_fill_manual(values = c("orgánica" = "#1f77b4", "química" = "#ff7f0e")) +
  labs(title = "Distribución de Tipos de Fertilización",
       subtitle = "Muestras válidas: Orgánica (754) - Química (726)",
       x = "Tipo de fertilización",
       y = "Cantidad de parcelas") +
  theme_pubr() +
  theme(legend.position = "none")

# Gráfico 2: Histograma del rendimiento del cultivo (versión mejorada)
hist_rendimiento <- ggplot(agua, aes(x = rendimiento_cultivo)) +
  geom_histogram(fill = "#2ca02c", color = "white", bins = 20) +
  labs(title = "Distribución del Rendimiento del Cultivo",
       x = "Rendimiento (toneladas)",
       y = "Frecuencia") +
  theme_pubr()

# Gráfico 3: Boxplot del rendimiento general
# Preparar datos para rendimiento general
rendimiento_data <- agua %>%
  filter(!is.na(rendimiento_cultivo)) %>%
  dplyr::select(rendimiento_cultivo)

# Calcular estadísticas
stats_rendimiento <- rendimiento_data %>%
  summarise(
    Media = mean(rendimiento_cultivo),
    Q1 = quantile(rendimiento_cultivo, 0.25),
    Q3 = quantile(rendimiento_cultivo, 0.75),
    IQR = IQR(rendimiento_cultivo)
  )

# Identificar outliers
outliers_rendimiento <- boxplot.stats(rendimiento_data$rendimiento_cultivo)$out
stats_rendimiento$Outliers <- length(outliers_rendimiento)

# Crear diagrama de caja individual
rendimiento_plot <- ggplot(rendimiento_data, aes(y = rendimiento_cultivo)) +
  geom_boxplot(
    fill = "white", 
    color = "black",
    outlier.color = "#e15759",
    outlier.size = 3
  ) +
  geom_point(aes(x = 0, y = stats_rendimiento$Media), 
             shape = 23, size = 4, fill = "yellow", color = "black") +
  geom_text(aes(x = 0, y = stats_rendimiento$Media, 
                label = sprintf("Media: %.2f", stats_rendimiento$Media)),
            vjust = -2, hjust = 0.5, size = 4, fontface = "bold") +
  labs(
    title = "Distribución del Rendimiento del Cultivo",
    subtitle = paste("Número de outliers:", stats_rendimiento$Outliers),
    y = "Rendimiento (toneladas)",
    caption = paste(
      "Q1:", round(stats_rendimiento$Q1, 2), "|",
      "Q3:", round(stats_rendimiento$Q3, 2), "|",
      "IQR:", round(stats_rendimiento$IQR, 2)
    )
  ) +
  theme_pubr() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray40"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

#GRÁFICO QQ RENDIMIENTO
qq_rendimiento_simple <- ggplot(agua, aes(sample = rendimiento_cultivo)) +
  stat_qq(color = "#1f77b4", size = 3, alpha = 0.7) +
  stat_qq_line(color = "#e15759", size = 1.2) +
  labs(
    title = "Prueba de Normalidad para Rendimiento del Cultivo",
    subtitle = "Gráfico Q-Q (Quantile-Quantile)",
    x = "Cuantiles Teóricos",
    y = "Cuantiles Muestrales"
  ) +
  theme_pubr() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray40")
  )

# ---------------------------------------------------------------
# 2. COMPARACIÓN ENTRE GRUPOS
# ---------------------------------------------------------------

# Filtrar datos válidos
agua_filtrada <- agua %>%
  filter(fertilizacion_utilizada %in% c("orgánica", "química"))

# Gráfico 4: Boxplot comparativo
# Preparar datos para comparación
rendimiento_fertilizacion_data <- agua %>%
  filter(fertilizacion_utilizada %in% c("orgánica", "química") &
           !is.na(rendimiento_cultivo)) %>%
  dplyr::select(fertilizacion_utilizada, rendimiento_cultivo)

# Calcular promedios por grupo
promedios_fertilizacion <- rendimiento_fertilizacion_data %>%
  group_by(fertilizacion_utilizada) %>%
  summarise(Media = mean(rendimiento_cultivo))

# Calcular outliers por grupo
outliers_data <- rendimiento_fertilizacion_data %>%
  group_by(fertilizacion_utilizada) %>%
  mutate(
    Q1 = quantile(rendimiento_cultivo, 0.25),
    Q3 = quantile(rendimiento_cultivo, 0.75),
    IQR = IQR(rendimiento_cultivo),
    Outlier = rendimiento_cultivo < (Q1 - 1.5 * IQR) | 
      rendimiento_cultivo > (Q3 + 1.5 * IQR)
  ) %>%
  filter(Outlier)

# Contar outliers por grupo
outliers_count <- outliers_data %>%
  group_by(fertilizacion_utilizada) %>%
  summarise(Outliers = n())

# Crear subtítulo con conteo de outliers
subtitle_comp <- paste(
  "Outliers: Orgánica =", outliers_count$Outliers[outliers_count$fertilizacion_utilizada == "orgánica"],
  "| Química =", outliers_count$Outliers[outliers_count$fertilizacion_utilizada == "química"]
)

# Crear diagrama de caja comparativo
comparativo_plot <- ggplot(rendimiento_fertilizacion_data, 
                           aes(x = fertilizacion_utilizada, 
                               y = rendimiento_cultivo,
                               fill = fertilizacion_utilizada)) +
  geom_boxplot(alpha = 0.8, outlier.color = "#e15759", outlier.size = 3) +
  geom_point(data = promedios_fertilizacion, aes(y = Media), 
             shape = 23, size = 4, fill = "yellow", color = "black") +
  geom_text(data = promedios_fertilizacion, 
            aes(y = Media, label = sprintf("Media: %.2f", Media)),
            vjust = -2, hjust = 0.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("orgánica" = "#1f77b4", "química" = "#ff7f0e")) +
  labs(
    title = "Rendimiento por Tipo de Fertilización",
    subtitle = subtitle_comp,
    y = "Rendimiento (toneladas)",
    x = "Tipo de Fertilización",
    caption = "Puntos amarillos = Media de cada grupo"
  ) +
  theme_pubr() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray40")
  )

# Gráfico 5: QQ-Plot para fertilización orgánica
qq_organica <- agua_filtrada %>%
  filter(fertilizacion_utilizada == "orgánica") %>%
  ggplot(aes(sample = rendimiento_cultivo)) +
  stat_qq(color = "#1f77b4", size = 2) +
  stat_qq_line(color = "red", size = 1) +
  labs(title = "QQ-Plot: Fertilización Orgánica",
       subtitle = "Prueba de normalidad",
       x = "Cuantiles Teóricos",
       y = "Cuantiles Muestrales") +
  theme_pubr()

# Gráfico 6: QQ-Plot para fertilización química
qq_quimica <- agua_filtrada %>%
  filter(fertilizacion_utilizada == "química") %>%
  ggplot(aes(sample = rendimiento_cultivo)) +
  stat_qq(color = "#ff7f0e", size = 2) +
  stat_qq_line(color = "red", size = 1) +
  labs(title = "QQ-Plot: Fertilización Química",
       subtitle = "Prueba de normalidad",
       x = "Cuantiles Teóricos",
       y = "Cuantiles Muestrales") +
  theme_pubr()

# ---------------------------------------------------------------
# 3. ANÁLISIS ESTADÍSTICO
# ---------------------------------------------------------------

# Separar los rendimientos
renOrg <- agua_filtrada %>% 
  filter(fertilizacion_utilizada == "orgánica") %>% 
  pull(rendimiento_cultivo)

renQuim <- agua_filtrada %>% 
  filter(fertilizacion_utilizada == "química") %>% 
  pull(rendimiento_cultivo)

# Medias
muO <- mean(renOrg)
muQ <- mean(renQuim)

# Gráfico 7: Comparación de medias
medias_plot <- data.frame(
  Tipo = c("Orgánica", "Química"),
  Rendimiento = c(muO, muQ)
) %>%
  ggplot(aes(x = Tipo, y = Rendimiento, fill = Tipo)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = round(Rendimiento, 2)), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("Orgánica" = "#1f77b4", "Química" = "#ff7f0e")) +
  labs(title = "Rendimiento Promedio por Tipo de Fertilización",
       y = "Rendimiento promedio (toneladas)") +
  theme_pubr() +
  theme(legend.position = "none")

# Prueba de varianzas
var_test <- var.test(x = renOrg, y = renQuim, alternative = "two.sided", conf.level = 0.9)

# Prueba t de hipótesis
t_test <- t.test(x = renOrg, y = renQuim, 
                 alternative = "two.sided",
                 mu = 0.15, 
                 paired = FALSE, 
                 var.equal = TRUE)

# ---------------------------------------------------------------
# MOSTRAR TODOS LOS GRÁFICOS
# ---------------------------------------------------------------

# Mostrar gráficos de análisis exploratorio
print(fert_plot)
print(hist_rendimiento)
print(rendimiento_plot)
print(qq_rendimiento_simple)

# Mostrar gráficos comparativos
print(comparativo_plot)
print(medias_plot)

# Mostrar pruebas de normalidad
print(qq_organica)
print(qq_quimica)

# Resultados estadísticos
cat("--- Resultado de la prueba de varianzas ---\n")
print(var_test)

cat("\n--- Resultado de la prueba t de hipótesis ---\n")
print(t_test)
